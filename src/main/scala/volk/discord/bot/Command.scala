package volk.discord.bot

import cats.effect.IO
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request, Uri}
import volk.discord.bot.Command.baseUri
import volk.discord.gateway.data.Status.Status
import volk.discord.gateway.data.{Activity, Emoji, Snowflake}
import volk.discord.gateway.payload.DiscordPayload
import volk.discord.gateway.payload.internal.InternalDisconnect
import volk.discord.gateway.payload.request.UpdatePresence

object Command {
  val baseUri = uri"https://discord.com/api/v10"
}

sealed trait Command

sealed trait ApiCommand extends Command {
  def uri: Uri
  def body: Json
  def method: Method
  def toRequest[F[_]]: Request[F] =
    Request[F](uri = uri, method = method).withEntity(body).putHeaders(`Content-Type`(MediaType.application.json))
}

sealed trait GatewayCommand extends Command {
  def toPayload: DiscordPayload
}

case object Disconnect extends GatewayCommand {
  def toPayload: DiscordPayload = InternalDisconnect(false)
}

sealed trait Embed
case class RichText(
    title: String,
    description: String
) extends Embed

case class Image(
    image: ImageEmbed
) extends Embed

/** @param url source url of image (only supports http(s) and attachments)
  * @param proxy_url a proxied url of the image
  * @param height height of image
  * @param width width of image
  */
case class ImageEmbed(
    url: String,
    proxy_url: Option[String],
    height: Option[Int],
    width: Option[Int]
)

case class CreateMessage(channelId: Snowflake, message: Message)
    extends ApiCommand {
  lazy val uri: Uri = baseUri / "channels" / channelId / "messages"
  lazy val method: Method = Method.POST
  lazy val body: Json = message.asJson.withObject(_.values.head)
}

sealed trait Message
case class SimpleMessage(content: String, tts: Boolean = false) extends Message
case class EmbedMessage(embeds: List[Embed]) extends Message
case class StickerMessage(sticker_ids: List[Snowflake]) extends Message

case class CreateReaction(channelId: Snowflake, messageId: Snowflake, emoji: Emoji) extends ApiCommand {
  lazy val uri: Uri = baseUri / "channels" / channelId / "messages" / messageId / "reactions" / Uri.encode(emoji.asJson.noSpaces) / "@me"
  lazy val body: Json = ???
  lazy val method: Method = ???
}

case class SetPresence(activity: Activity, status: Status, afk: Boolean) extends GatewayCommand {
  override def toPayload: DiscordPayload = UpdatePresence(
    None,
    activity :: Nil,
    status,
    afk
  )
}


