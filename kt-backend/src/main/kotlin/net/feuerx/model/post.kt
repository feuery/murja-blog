package net.feuerx.model

import java.time.LocalDateTime
import kotlinx.serialization.Serializable

import net.feuerx.hacks.DateTimeSerializer

@Serializable
data class Post (val id: Int,
		 val title: String,
		 val content: String,
		 val creator: User,
		 val tags: List<String>,
 
		 @Serializable(with = DateTimeSerializer::class)
		 val created_at: LocalDateTime)

@Serializable
data class Title ( val Title: String
		 , val Month: Long
		 , val Year: Long
		 , val Id: Long
		 , val Tags: List<String>)
