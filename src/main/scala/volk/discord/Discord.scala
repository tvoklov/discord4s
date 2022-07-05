package volk.discord

import cats.effect.IO
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

  import scribe.cats.{io => scribe}

  def simpleBot(
      config: DiscordBotConfig
  )(receiveThrough: PartialFunction[Event, IO[List[Command]]]): IO[Unit] =
    bot(config, None)(receiveThrough)

  def concurrentBot(config: DiscordBotConfig, commandTopic: Topic[IO, Command])(
      receiveThrough: PartialFunction[Event, IO[List[Command]]]
  ): IO[Unit] =
    bot(config, Some(commandTopic))(receiveThrough)

  private def bot(
      config: DiscordBotConfig,
      outsideTopic: Option[Topic[IO, Command]]
  )(
      receiveThrough: PartialFunction[Event, IO[List[Command]]]
  ): IO[Unit] = {
    for {
      t <- Topic[IO, DiscordPayload]
      _ <- JdkHttpClient
        .simple[IO]
        .use { client =>
          def handleCommand: Command => IO[Boolean] = {
            case c: GatewayCommand =>
              t.publish1(c.toPayload).map(_.isRight)
            case command: ApiCommand =>
              val req = command.toRequest.putHeaders(
                Header.Raw(Authorization.name, config.botToken)
              )
              client.expect[Json](req).attempt.flatMap {
                case Left(value) =>
                  scribe
                    .error(s"command $command failed: ${value.getMessage}")
                    .as(false)
                case Right(_) => IO.pure(true)
              }
          }

          val gateway =
            Gateway
              .createAndRunGateway[Int](config)(
                t,
                {
                  case (s, p: Event) =>
                    receiveThrough
                      .applyOrElse[Event, IO[List[Command]]](
                        p,
                        _ => IO.pure(Nil)
                      )
                      .flatMap(_.traverse(handleCommand))
                      .as(s -> Nil)
                  case (s, _) => IO.pure(s -> Nil)
                }
              )(0)

          outsideTopic
            .map(_.subscribe(10).evalMap(handleCommand).compile.drain)
            .fold(gateway)(gateway &> _)
        }
    } yield ()
  }

}
