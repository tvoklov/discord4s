package volk.discord.gateway

import io.circe._
import io.circe.syntax._
import io.circe.generic.auto._
import volk.discord.gateway.payload.HeartbeatInterval
import volk.discord.gateway.payload.internal.Heartbeat

private case class InternalDiscordState(
    sessionId: Option[String],
    heartbeatInterval: Option[HeartbeatInterval],
    seqNum: Option[Int]
) {
  def withHeartbeatInterval(
      heartbeatInterval: HeartbeatInterval
  ): InternalDiscordState =
    copy(heartbeatInterval = Some(heartbeatInterval))

  def newSession(sessionId: String): InternalDiscordState =
    copy(sessionId = Some(sessionId))

  def withHeartbeatSequenceNum(seqNum: Int): InternalDiscordState =
    copy(seqNum = Some(seqNum))

  def incHeartbeatSequenceNum: InternalDiscordState =
    copy(seqNum = seqNum.map(_ + 1))

  def heartbeatMsg: Json = seqNum match {
    case Some(value) => Heartbeat(value).asJson
    case None        => Heartbeat.firstHeartbeat
  }
}
