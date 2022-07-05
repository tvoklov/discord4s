package volk.discord.gateway

/** @see [[https://discord.com/developers/docs/topics/gateway#list-of-intents]] */
object Intent {

  type Intent = Int

  val GUILDS: Intent = 1 << 0
  val GUILD_MEMBERS: Intent = 1 << 1
  val GUILD_BANS: Intent = 1 << 2
  val GUILD_EMOJIS_AND_STICKERS: Intent = 1 << 3
  val GUILD_INTEGRATIONS: Intent = 1 << 4
  val GUILD_WEBHOOKS: Intent = 1 << 5
  val GUILD_INVITES: Intent = 1 << 6
  val GUILD_VOICE_STATES: Intent = 1 << 7
  val GUILD_PRESENCES: Intent = 1 << 8
  val GUILD_MESSAGES: Intent = 1 << 9
  val GUILD_MESSAGE_REACTIONS: Intent = 1 << 10
  val GUILD_MESSAGE_TYPING: Intent = 1 << 11
  val GUILD_SCHEDULED_EVENTS: Intent = 1 << 16

  val DIRECT_MESSAGES: Intent = 1 << 12
  val DIRECT_MESSAGE_REACTIONS: Intent = 1 << 13
  val DIRECT_MESSAGE_TYPING: Intent = 1 << 14

  val MESSAGE_CONTENT: Intent = 1 << 15

  val AUTO_MODERATION_CONFIGURATION: Intent = 1 << 20
  val AUTO_MODERATION_EXECUTION: Intent = 1 << 21


  implicit class IntentSum(intent: Intent) {
    def +++(otherIntent: Intent): Intent = intent | otherIntent
  }

}
