package volk.discord.gateway

case class DiscordBotConfig(
    token: String,
    intents: Int
) {
  def botToken: String = "Bot " + token
}
