package volk.discord.gateway

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.client.websocket.WSFrame
import volk.discord.gateway.data.Status.Status
import volk.discord.gateway.data._
import volk.discord.gateway.payload.event._
import volk.discord.gateway.payload.internal._
import volk.util.CaseConverter

object payload {

  type HeartbeatInterval = Int

  type OPCode = Int

  object DiscordPayload {
    val messageParser: Decoder[DiscordPayload] =
      (c: HCursor) => {
        for {
          op <- c.downField("op").as[Int]
          parsed <-
            if (op > 0) c.as[InternalPayload](internal.decoder)
            else if (op == 0) c.as[Event](event.decoder)
            else
              Left(
                DecodingFailure(
                  DecodingFailure.Reason.CustomReason(s"op code = $op"),
                  Nil
                )
              )
        } yield parsed
      }

    val messageEncoder: Encoder[DiscordPayload] =
      (a: DiscordPayload) => {
        a.opCode match {
          case -1 => a.asJson
          case op =>
            JsonObject(
              "op" -> Json.fromInt(op),
              // oh no this is bad this is a bad line
              "d" -> a.asJson.withObject(_.values.head)
            ).asJson
        }
      }

    def fromWebsocketFrame: String => Either[Throwable, DiscordPayload] =
      jawn.parse(_).flatMap(_.as[DiscordPayload](messageParser))

    def toWebsocketFrame: DiscordPayload => WSFrame.Text = idp =>
      WSFrame.Text(idp.asJson(messageEncoder).noSpaces)
  }

  sealed trait DiscordPayload {
    def toWebsocketFrame: WSFrame.Text =
      DiscordPayload.toWebsocketFrame(this)

    def opCode: OPCode
  }

  object internal {
    sealed trait InternalPayload extends DiscordPayload

    val decoder: Decoder[InternalPayload] = (c: HCursor) =>
      for {
        op <- c.downField("op").as[Int]
        parsed <- {
          val d = c.downField("d")
          op match {
            case Hello.opCode          => d.as[Hello]
            case Heartbeat.opCode      => d.as[Heartbeat]
            case HeartbeatAck.opCode   => Right(HeartbeatAck())
            case Identify.opCode       => d.as[Identify]
            case Resume.opCode         => d.as[Resume]
            case Reconnect.opCode      => Right(Reconnect())
            case InvalidSession.opCode => d.as[Boolean].map(InvalidSession(_))
            case _                     => Right(UnknownPayload(op))
          }
        }
      } yield parsed

    // https://discord.com/developers/docs/topics/gateway#connecting-example-gateway-hello
    object Hello { lazy val opCode = 10 }
    case class Hello(heartbeat_interval: HeartbeatInterval)
        extends InternalPayload {
      val opCode: OPCode = Hello.opCode
    }

    // https://discord.com/developers/docs/topics/gateway#identifying
    object Identify {
      lazy val opCode = 2

      case class Properties(
          os: String,
          browser: String,
          device: String
      )
    }
    case class Identify(
        token: String,
        intents: Int,
        properties: Identify.Properties
    ) extends InternalPayload {
      val opCode: OPCode = Identify.opCode
    }

    // https://discord.com/developers/docs/topics/gateway#heartbeating
    object Heartbeat {
      lazy val opCode = 1

      val firstHeartbeat: Json =
        Json.obj(
          "op" -> Json.fromInt(opCode),
          "d" -> Json.Null
        )
    }
    case class Heartbeat(seqNumber: Int) extends InternalPayload {
      val opCode: OPCode = Heartbeat.opCode
    }

    // https://discord.com/developers/docs/topics/gateway#heartbeating
    object HeartbeatAck {
      lazy val opCode = 11
    }
    case class HeartbeatAck() extends InternalPayload {
      val opCode: OPCode = HeartbeatAck.opCode
    }

    // https://discord.com/developers/docs/topics/gateway#resuming
    object Resume { lazy val opCode = 6 }
    case class Resume(
        token: String,
        session_id: String,
        seq: Int
    ) extends InternalPayload {
      val opCode: OPCode = Resume.opCode
    }

    object Resumed { lazy val opCode = 7 }
    case class Resumed() extends InternalPayload {
      val opCode: OPCode = Resumed.opCode
    }

    // https://discord.com/developers/docs/topics/gateway#reconnect
    object Reconnect { lazy val opCode = 7 }
    case class Reconnect() extends InternalPayload {
      val opCode: OPCode = Reconnect.opCode
    }

    // https://discord.com/developers/docs/topics/gateway#invalid-session
    object InvalidSession { lazy val opCode = 9 }
    case class InvalidSession(resumable: Boolean) extends InternalPayload {
      val opCode: OPCode = InvalidSession.opCode
    }

    case class InternalDisconnect(shouldReconnect: Boolean)
        extends InternalPayload
        with NoOpCode

    case class UnknownPayload(opCode: OPCode) extends InternalPayload
  }

  object event {

    sealed trait Event extends DiscordPayload {
      lazy val opCode: OPCode = 0
    }

    trait EventTypeGenerated {
      lazy val eventType: String =
        CaseConverter
          .camelCaseToSnakeCase(
            this.getClass.getSimpleName.replace("$", "")
          )
          .toUpperCase
    }

    val decoder: Decoder[Event] = (c: HCursor) => {
      val d = c.downField("d")
      for {
        eType <- c.downField("t").as[String]
        parsed <- eType match {
          case Ready.eventType => d.as[Ready]

          case GuildCreate.eventType => d.as[GuildCreate]
          case GuildUpdate.eventType => d.as[Guild].map(GuildUpdate(_))
          case GuildDelete.eventType => d.as[GuildCreate]

          case GuildBanAdd.eventType    => d.as[GuildBanAdd]
          case GuildBanRemove.eventType => d.as[GuildBanRemove]

          case ChannelCreate.eventType => d.as[Channel].map(ChannelCreate(_))
          case ChannelUpdate.eventType => d.as[Channel].map(ChannelUpdate(_))
          case ChannelDelete.eventType => d.as[Channel].map(ChannelDelete(_))

          case ThreadCreate.eventType => d.as[Channel].map(ThreadCreate(_))
          case ThreadUpdate.eventType => d.as[Channel].map(ThreadUpdate(_))
          case ThreadDelete.eventType =>
            d.as[DeletedThread].map(ThreadDelete(_))

          case MessageCreate.eventType => d.as[Message].map(MessageCreate(_))
          case MessageUpdate.eventType => d.as[Message].map(MessageUpdate(_))
          case MessageDelete.eventType => d.as[MessageDelete]

          case MessageReactionAdd.eventType    => d.as[MessageReactionAdd]
          case MessageReactionRemove.eventType => d.as[MessageReactionRemove]

          case TypingStart.eventType => d.as[TypingStart]

          case _ => Right(UnknownEvent(eType))
        }
      } yield parsed
    }

    object GuildCreate extends EventTypeGenerated
    case class GuildCreate(
        joined_at: Timestamp,
        large: Boolean,
        unavailable: Boolean,
        member_count: Int,
        members: List[Member],
        channels: List[Channel]
    ) extends Event

    object GuildUpdate extends EventTypeGenerated
    case class GuildUpdate(guild: Guild) extends Event

    object GuildDelete extends EventTypeGenerated
    case class GuildDelete(id: Snowflake, unavailable: Boolean) extends Event

    object GuildBanAdd extends EventTypeGenerated
    case class GuildBanAdd(guild_id: Int, user: User) extends Event

    object GuildBanRemove extends EventTypeGenerated
    case class GuildBanRemove(guild_id: Int, user: User) extends Event

    object TypingStart extends EventTypeGenerated
    case class TypingStart(
        channel_id: Snowflake,
        guild_id: Option[Snowflake],
        user_id: Snowflake,
        timestamp: Int,
        member: Option[Member]
    ) extends Event

    object MessageCreate extends EventTypeGenerated
    case class MessageCreate(message: Message) extends Event

    object MessageUpdate extends EventTypeGenerated
    case class MessageUpdate(message: Message) extends Event

    object MessageDelete extends EventTypeGenerated
    case class MessageDelete(
        id: Snowflake,
        channel_id: Snowflake,
        guild_id: Option[Snowflake]
    ) extends Event

    object MessageReactionAdd extends EventTypeGenerated
    case class MessageReactionAdd(
        user_id: Snowflake,
        channel_id: Snowflake,
        message_id: Snowflake,
        guild_id: Option[Snowflake],
        member: Option[Member],
        emoji: Emoji
    ) extends Event

    object MessageReactionRemove extends EventTypeGenerated
    case class MessageReactionRemove(
        user_id: Snowflake,
        channel_id: Snowflake,
        message_id: Snowflake,
        guild_id: Option[Snowflake],
        emoji: Emoji
    ) extends Event

    object Ready extends EventTypeGenerated
    case class Ready(v: Int, session_id: String, user: User) extends Event

    object ChannelCreate extends EventTypeGenerated
    case class ChannelCreate(channel: Channel) extends Event

    object ChannelUpdate extends EventTypeGenerated
    case class ChannelUpdate(channel: Channel) extends Event

    object ChannelDelete extends EventTypeGenerated
    case class ChannelDelete(channel: Channel) extends Event

    object ThreadCreate extends EventTypeGenerated
    case class ThreadCreate(channel: Channel) extends Event

    object ThreadUpdate extends EventTypeGenerated
    case class ThreadUpdate(channel: Channel) extends Event

    object ThreadDelete extends EventTypeGenerated
    case class ThreadDelete(channel: DeletedThread) extends Event

    object ChannelPinsUpdate extends EventTypeGenerated
    case class ChannelPinsUpdate(
        guild_id: Option[Snowflake],
        channel_id: Snowflake,
        last_pin_timestamp: Option[String]
    )

    case class UnknownEvent(eventType: String) extends Event

  }

  object request {
    sealed trait Request extends DiscordPayload

    object RequestGuildMembers { lazy val opCode: OPCode = 8 }
    case class RequestGuildMembers(
        guild_id: Snowflake,
        limit: Int = 0
    ) extends Request {
      val opCode: OPCode = RequestGuildMembers.opCode
    }

    object UpdateVoiceState { lazy val opCode = 4 }
    case class UpdateVoiceState(
        guild_id: Snowflake,
        channel_id: Snowflake,
        self_mute: Option[Boolean],
        self_def: Option[Boolean]
    ) extends Request {
      val opCode: OPCode = UpdateVoiceState.opCode
    }

    object UpdatePresence { lazy val opCode = 3 }
    case class UpdatePresence(
        since: Option[Int],
        activities: List[Activity],
        status: Status,
        afk: Boolean
    ) extends Request {
      val opCode: OPCode = UpdatePresence.opCode
    }

  }

  sealed trait NoOpCode {
    def opCode: Int = -1
  }

}
