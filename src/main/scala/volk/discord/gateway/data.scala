package volk.discord.gateway

object data {

  type Snowflake = String
  type Query = String

  type Timestamp = String

  case class Activity(
      name: String,
      `type`: ActivityType.ActivityType,
      url: Option[String]
  )

  case class Message(
      id: Snowflake,
      channel_id: Snowflake,
      author: User,
      content: String,
      timestamp: String
  )

  case class Emoji(
      id: Option[Snowflake],
      name: String
  )

  case class Member(
      user: Option[User],
      nick: Option[String],
      roles: Array[Snowflake],
      joined_at: Option[String],
      deaf: Boolean
  )

  case class User(
      id: Snowflake,
      username: String,
      discriminator: String,
      private val bot: Option[Boolean],
      email: Option[String]
  ) {
    lazy val isBot: Boolean = bot.contains(true)
  }

  case class Guild(
      id: Snowflake,
      name: String,
      owner_id: Snowflake,
      afk_channel_id: Snowflake,
      afk_timeout: Int,
      verification_level: VerificationLevel.VerificationLevel,
      system_channel_id: Snowflake,
      rules_channel_id: Snowflake,
      description: String,
      premium_tier: PremiumTier.PremiumTier,
      preferred_locale: String,
      public_updates_channel_id: Snowflake
  )

  case class Channel(
      id: Snowflake,
      private val `type`: ChannelType.ChannelType,
      guild_id: Option[Snowflake],
      name: Option[String],
      topic: Option[String],
      parent_id: Option[String],
      private val nsfw: Option[Boolean]
  ) {
    val channelType: ChannelType.ChannelType = `type`
    val isNsfw: Boolean = nsfw.contains(true)
  }

  case class DeletedThread(
      id: Snowflake,
      guild_id: Option[Snowflake],
      parent_id: Snowflake,
      private val `type`: ChannelType.ChannelType
  ) {
    val channelType: ChannelType.ChannelType = `type`
  }

  object Status {
    type Status = String

    val online = "online"
    val doNotDisturb = "dnd"
    val idle = "idle"
    val invisible = "invisible"
    val offline = "offline"
  }

  object ActivityType {
    type ActivityType = Int

    /** Playing {name}, eg: "Playing Rocket League" */
    val Game = 0

    /** Streaming {details}, eg: "Streaming Rocket League" */
    val Streaming = 1

    /** Listening to {name}, eg: "Listening to Spotify" */
    val Listening = 2

    /** Watching {name}, eg: "Watching YouTube Together" */
    val Watching = 3

    /** {emoji} {name}, eg: ":smiley: I am cool" */
    val Custom = 4

    /** Competing in {name}, eg: "Competing in Arena World Champions" */
    val Competing = 5
  }

  object VerificationLevel {
    type VerificationLevel = Int

    /** unrestricted */
    val NONE = 0

    /** must have verified email on account */
    val LOW = 1

    /** must be registered on Discord for longer than 5 minutes */
    val MEDIUM = 2

    /** must be a member of the server for longer than 10 minutes */
    val HIGH = 3

    /** must have a verified phone number */
    val VERY_HIGH = 4
  }

  object NSFWLevel {
    type NSFWLevel = Int

    val DEFAULT = 0
    val EXPLICIT = 1
    val SAFE = 2
    val AGE_RESTRICTED = 3
  }

  object PremiumTier {
    type PremiumTier = Int

    /** guild has not unlocked any Server Boost perks */
    val NONE = 0

    /** guild has unlocked Server Boost level 1 perks */
    val TIER_1 = 1

    /** guild has unlocked Server Boost level 2 perks */
    val TIER_2 = 2

    /** guild has unlocked Server Boost level 3 perks */
    val TIER_3 = 3
  }

  object ChannelType {
    type ChannelType = Int

    /** a text channel within a server */
    val GUILD_TEXT = 0

    /** a direct message between users */
    val DM = 1

    /** a voice channel within a server */
    val GUILD_VOICE = 2

    /** a direct message between multiple users */
    val GROUP_DM = 3

    /** an organizational category that contains up to 50 channels */
    val GUILD_CATEGORY = 4

    /** a channel that users can follow and crosspost into their own server */
    val GUILD_NEWS = 5

    /** a temporary sub-channel within a GUILD_NEWS channel */
    val GUILD_NEWS_THREAD = 10

    /** a temporary sub-channel within a GUILD_TEXT channel */
    val GUILD_PUBLIC_THREAD = 11

    /** a temporary sub-channel within a GUILD_TEXT channel that is only viewable by those invited and those with the MANAGE_THREADS permission */
    val GUILD_PRIVATE_THREAD = 12

    /** a voice channel for hosting events with an audience */
    val GUILD_STAGE_VOICE = 13

    /** the channel in a hub containing the listed servers */
    val GUILD_DIRECTORY = 14

    /** (still in development) a channel that can only contain threads */
    val GUILD_FORUM = 15
  }

}
