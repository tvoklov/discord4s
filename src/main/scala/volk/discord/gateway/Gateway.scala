package volk.discord.gateway

import cats.Monad
import cats.effect.kernel.Async
import cats.effect.{Concurrent, IO, Ref}
import cats.implicits._
import fs2.concurrent.{SignallingRef, Topic}
import org.http4s.client.websocket.{WSFrame, WSRequest}
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.{JdkHttpClient, JdkWSClient}
import org.http4s.implicits._
import org.http4s.{Header, Headers, Method, Request, Uri}
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityDecoder._
import volk.discord.gateway.payload.DiscordPayload._
import volk.discord.gateway.payload._

import scala.concurrent.duration.DurationInt

object Gateway {
  import scribe.{cats => scribe}

  private object uri {
    val gatewayBot = uri"https://discord.com/api/v10/gateway/bot"
  }

  private case class GatewayAddressResponse(
      url: String
  )

  def createAndRunGateway[F[_]: Async, S](config: DiscordBotConfig)(
      sendFrom: Topic[F, DiscordPayload],
      receiveThrough: PartialFunction[(S, DiscordPayload), F[
        (S, List[DiscordPayload])
      ]]
  )(zeroState: S): F[Unit] = {
    for {
      uri <-
        JdkHttpClient
          .simple[F]
          .use(
            _.expect[GatewayAddressResponse](
              Request[F](
                uri = uri.gatewayBot,
                headers = Headers(
                  Header.Raw(Authorization.name, config.botToken)
                ),
                method = Method.GET
              )
            )
              .map(_.url)
              .map(Uri.fromString)
              .rethrow
          )

      _ <- runGateway(uri, config)(sendFrom, receiveThrough)(zeroState)
    } yield ()
  }

  def runGateway[F[_]: Async, S](uri: Uri, config: DiscordBotConfig)(
      sendFrom: Topic[F, DiscordPayload],
      receiveThrough: PartialFunction[(S, DiscordPayload), F[
        (S, List[DiscordPayload])
      ]]
  )(zeroState: S): F[Unit] = {
    val scribeF = scribe[F]
    for {
      intState <- SignallingRef[F, InternalDiscordState](
        InternalDiscordState(None, None, None)
      )
      outerState <- Ref.of[F, S](zeroState)

      stopAndReconnect <- SignallingRef[F, (Boolean, Boolean)]((false, true))

      _ <- {
        def process: PartialFunction[DiscordPayload, F[
          List[DiscordPayload]
        ]] = {
          case internal.Hello(heartbeatInterval) =>
            for {
              _ <- intState.update(_.withHeartbeatInterval(heartbeatInterval))
            } yield List(
              internal.Identify(
                config.botToken,
                config.intents,
                internal.Identify.Properties("linux", "discord4s", "discord4s")
              )
            )

          case internal.Heartbeat(sn) =>
            for {
              _ <- intState.update(_.withHeartbeatSequenceNum(sn + 1))
            } yield internal.Heartbeat(sn + 1) :: Nil

          case event.Ready(v, session_id, user) =>
            for {
              _ <- intState.update(_.newSession(session_id))
            } yield Nil

          case internal.Reconnect() =>
            for {
              _ <- intState.set(InternalDiscordState(None, None, None))
              _ <- stopAndReconnect.update(_.copy(_2 = true))
            } yield internal.InternalDisconnect(true) :: Nil

          case internal.HeartbeatAck() => Monad[F].pure(Nil)
        }

        JdkWSClient
          .simple[F]
          .flatMap(_.connectHighLevel(WSRequest(uri)))
          .use { conn =>
            val receive =
              conn.receiveStream
                .evalTap(frame =>
                  scribeF.debug("got frame from discord ", frame.toString)
                )
                .evalMapAccumulate(zeroState) {
                  case (state, WSFrame.Text(text, _)) =>
                    DiscordPayload.fromWebsocketFrame(text) match {
                      case Left(value) =>
                        scribeF
                          .error(s"Discord sent weird payload: $text", value)
                          .as(state -> List.empty[DiscordPayload])
                      case Right(payload) =>
                        process
                          .andThen(_.map(state -> _))
                          .orElse(
                            PartialFunction
                              .fromFunction[DiscordPayload, F[
                                (S, List[DiscordPayload])
                              ]](receiveThrough.compose(state -> _))
                          )
                          .apply(payload)
                    }
                  case (state, frame) =>
                    scribeF
                      .error(s"what am i supposed to do with $frame")
                      .as(state -> List.empty[DiscordPayload])
                }
                .evalMap { case (state, payloads) =>
                  payloads
                    .traverse {
                      case internal.InternalDisconnect(shouldReconnect) =>
                        for {
                          _ <- outerState.set(state)
                          _ <- stopAndReconnect.set((true, shouldReconnect))
                        } yield Option.empty[WSFrame.Text]
                      case payload =>
                        Monad[F].pure(
                          Option(
                            WSFrame
                              .Text(payload.asJson(messageEncoder).noSpaces)
                          )
                        )
                    }
                    .map(_.flatten)
                }
                .flatMap(fs2.Stream.emits)

            val heartbeat =
              intState.discrete
                .map(_.heartbeatInterval)
                .collect { case Some(heartbeat) => heartbeat }
                .take(1)
                .flatMap { heartbeat =>
                  fs2.Stream
                    .awakeEvery[F](heartbeat.milliseconds)
                    .evalMap(_ =>
                      for {
                        s <- intState.getAndUpdate(_.incHeartbeatSequenceNum)
                      } yield s.heartbeatMsg.noSpaces
                    )
                    .map(WSFrame.Text(_))
                }

            val send =
              sendFrom
                .subscribe(20)
                .map(_.asJson(messageEncoder).noSpaces)
                .map(WSFrame.Text(_))
                .merge(receive)
                .merge(heartbeat)
                .evalTap { frame =>
                  scribeF.debug("sent frame to discord ", frame.toString)
                }

            send.through(conn.sendPipe).compile.drain
          }
      }

      _ <-
        for {
          sar <- stopAndReconnect.get
          (_, reconnect) = sar
          r <-
            if (!reconnect) Async[F].unit
            else
              for {
                os <- outerState.get
                r <- runGateway(uri, config)(sendFrom, receiveThrough)(os)
              } yield r
        } yield r

    } yield ()
  }

}
