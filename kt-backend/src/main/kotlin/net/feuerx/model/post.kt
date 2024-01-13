package net.feuerx.model

import java.time.LocalDateTime
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json

import net.feuerx.hacks.DateTimeSerializer
import kotliquery.Session
import kotliquery.queryOf
import kotliquery.Row

@Serializable
data class Post (val id: Long,
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

fun dbToPost(session: Session, row: Row): Post {
    return Post(
            row.long("post_id"),
            row.string("title"),
            row.string("content"),
            getUserById(session, row.long("creator_id")),
            Json.decodeFromString<List<String>>(row.string("tags")),
            row.localDateTime("created_at")
    )
}


fun postById(session: Session, post_id: Long): Post {
    val show_hidden = false
    val posts = session.run(queryOf(
				"""
SELECT p.ID as post_id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, p.creator_id
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
WHERE p.ID = :post_id AND (NOT p.tags ?? 'hidden' OR (p.tags ?? 'hidden' AND :show_hidden))
GROUP BY p.ID, u.ID
		    """
		       , mapOf("post_id" to post_id,
			       "show_hidden" to show_hidden))
				.map {row -> dbToPost(session, row) }
				.asList)

    return posts.first()
}
