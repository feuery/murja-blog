package net.feuerx.model

import kotlinx.serialization.Serializable
import kotlinx.serialization.SerialName

@Serializable
data class PageResponse(val posts: List<Post>,
			val id: Long,

			@SerialName("last-page?")
			val last_page: Boolean)
