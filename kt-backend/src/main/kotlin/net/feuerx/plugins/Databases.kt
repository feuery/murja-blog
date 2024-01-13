package net.feuerx.plugins

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import java.sql.*
import kotlinx.coroutines.*
import kotlinx.serialization.json.Json
import kotliquery.sessionOf
import kotliquery.HikariCP
import kotliquery.queryOf
import kotlin.text.toLong
import net.feuerx.model.Post
import net.feuerx.model.Title
import net.feuerx.model.PageResponse
import net.feuerx.model.getUserById
import net.feuerx.model.dbToPost
import net.feuerx.model.postById

fun Application.configureDatabases() {
    routing {
	get ("/api/posts/titles") {
	    sessionOf(HikariCP.dataSource()).use { session ->
		val show_hidden = false;
		val titles =
		    session.run(
			queryOf(
			    """
SELECT p.Title,
EXTRACT(MONTH FROM p.created_at) AS "Month",
EXTRACT(YEAR FROM p.created_at) AS "Year",
p.id as "Id",
p.Tags
FROM blog.Post p
WHERE :show_hidden OR (NOT p.tags ?? 'unlisted' AND NOT p.tags ?? 'hidden')
ORDER BY p.created_at DESC
			    """
			       , mapOf("show_hidden" to show_hidden))
			    .map { row ->
				Title( row.string("title")
				     , row.long("month")
				     , row.long("year")
				     , row.long("id")
				     , Json.decodeFromString<List<String>>(row.string("tags")))}
			    .asList)

		call.respond(titles)
		
	    }
	}
        get("/api/posts/post/{post-id}") {
	    sessionOf(HikariCP.dataSource()).use { session ->
		val post_id_nil = call.parameters["post-id"]?.toLong()
		val post_id: Long = post_id_nil ?: error("invalid post id")
		call.respond(postById(session, post_id))
	    }
	}
	get("/api/posts/page/{page}/page-size/{size}") {
	    sessionOf(HikariCP.dataSource()).use { session ->
		val page_nil = call.parameters["page"]?.toLong()
		val pagesize_nil = call.parameters["size"]?.toLong()

		val page: Long = page_nil ?: error("page is required")
		val pagesize = pagesize_nil ?: error("pagesize is required")
		
		val posts: List<Post> =
                    session.run(
                        queryOf(
                            """
SELECT p.ID as post_id, p.Title, p.Content, p.created_at, p.tags, u.Username, u.Nickname, u.Img_location, p.creator_id
FROM blog.Post p
JOIN blog.Users u ON u.ID = p.creator_id
WHERE (((NOT p.tags ?? 'unlisted') OR :show_hidden)
   AND ((NOT p.tags ?? 'hidden') OR :show_hidden))
GROUP BY p.ID, u.ID
ORDER BY p.created_at DESC
LIMIT :pageSize
OFFSET :pageId
			    """,
                            mapOf(
                                "pageId" to page,
                                "pageSize" to pagesize,
                                "show_hidden" to false
                            )
                        )
                            .map {row -> dbToPost(session, row)}
                            .asList
                    )
		val response = PageResponse(posts, page, false)

		call.respond(response)
	    }
	}
    }
}
