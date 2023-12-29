package net.feuerx.model

import java.time.LocalDateTime
import kotlinx.serialization.Serializable

import net.feuerx.hacks.DateTimeSerializer

@Serializable
data class Post (val id: Int,
		 val title: String,
		 val content: String,
		 val creator: Int,
		 val tags: List<String>,
 
		 @Serializable(with = DateTimeSerializer::class)
		 val created_at: LocalDateTime)
