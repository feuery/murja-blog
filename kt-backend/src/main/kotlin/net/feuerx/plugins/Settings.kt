package net.feuerx.plugins

import kotlinx.serialization.Serializable
import kotlinx.serialization.SerialName
import kotlinx.serialization.json.Json
import java.io.File

/*
    "client-config": {
    "time-format": "dd.MM.yyyy HH:mm",
    "blog-title": "Murja.dev @ $HOSTNAME",
    "recent-post-count": 6,
    "xss-filter-posts?": false
  }
 */

@Serializable
data class ClientConfig( @SerialName("time-format")
			 val time_format: String 
		       , @SerialName("blog-title")
			 val blog_title: String 
		       , @SerialName("recent-post-count")
		         val recent_post_count: Long 
		       , @SerialName("xss-filter-posts?")
		         val xss_filter_posts: Boolean)

@Serializable
data class Settings(
    @SerialName("client-config")
    val client_conf: ClientConfig)

val settings = lazy {
    val f = File("/etc/murja/config.json")
    Json { ignoreUnknownKeys = true }.decodeFromString<Settings>(
	f.readText()
    )
}
