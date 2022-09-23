package volk.discord

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.kernel.Async
import cats.implicits.toTraverseOps
import fs2.concurrent.Topic
import io.circe.Json
import org.http4s._
import org.http4s.circe._
import org.http4s.headers.Authorization
import org.http4s.jdkhttpclient.JdkHttpClient
import volk.discord.bot.{ApiCommand, Command, GatewayCommand}
import volk.discord.gateway.payload.DiscordPayload
import volk.discord.gateway.payload.event.Event
import volk.discord.gateway.{DiscordBotConfig, Gateway}

object Discord {

  import scribe.{cats => scribe}

  type EventProcessor[F[_]] = PartialFunction[Event, F[List[Command]]]

  def simpleBot[F[_]: Async: Concurrent: Parallel](
      config: DiscordBotConfig
  )(receiveThrough: EventProcessor[F]): F[Unit] =
    bot(config, None)(receiveThrough)

  def concurrentBot[F[_]: Async: Concurrent: Parallel](
      config: DiscordBotConfig,
      commandTopic: Topic[F, Command]
  )(
      receiveThrough: EventProcessor[F]
  ): F[Unit] =
    bot(config, Some(commandTopic))(receiveThrough)

  private def bot[F[_]: Async: Concurrent: Parallel](
      config: DiscordBotConfig,
      outsideTopic: Option[Topic[F, Command]]
  )(
      receiveThrough: EventProcessor[F]
  ): F[Unit] = {
    val scribeF = scribe[F]
    for {
      t <- Topic[F, DiscordPayload]
      _ <-
        JdkHttpClient
          .simple[F]
          .use { client =>
            def handleCommand: Command => F[Boolean] = {
              case c: GatewayCommand =>
                t.publish1(c.toPayload).map(_.isRight)
              case command: ApiCommand =>
                val req =
                  command
                    .toRequest[F]
                    .putHeaders(
                      Header.Raw(Authorization.name, config.botToken)
                    )
                client.expect[Json](req).attempt.flatMap {
                  case Left(value) =>
                    scribeF
                      .error(s"command $command failed: ${value.getMessage}")
                      .as(false)
                  case Right(_) => Monad[F].pure(true)
                }
            }

            val gateway =
              Gateway
                .createAndRunGateway[F, Int](config)(
                  t,
                  {
                    case (s, p: Event) =>
                      receiveThrough
                        .applyOrElse[Event, F[List[Command]]](
                          p,
                          _ => Monad[F].pure(Nil)
                        )
                        .flatMap(_.traverse(handleCommand))
                        .as(s -> Nil)
                    case (s, _) => Monad[F].pure(s -> Nil)
                  }
                )(0)

            outsideTopic
              .map(_.subscribe(10).evalMap(handleCommand).compile.drain)
              .fold(gateway)(gateway &> _)
          }
    } yield ()
  }

}
