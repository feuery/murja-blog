package net.feuerx.plugins

import io.ktor.http.*
import io.ktor.server.application.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import java.sql.*
import kotlinx.coroutines.*
import kotliquery.sessionOf
import kotliquery.HikariCP
import kotliquery.queryOf
import kotlin.text.toLong
import net.feuerx.model.Post

fun Application.configureDatabases() {
	routing {
	    get("/api/posts/page/{page}/page-size/{size}") {
		    sessionOf(HikariCP.dataSource()).use { session ->
		val page_nil = call.parameters["page"]?.toLong()
		val pagesize_nil = call.parameters["size"]?.toLong()

		val page: Long = page_nil?: error("page is required")
		val pagesize = pagesize_nil?: error("pagesize is required")

		println("Page: " + page + ", pagesize: " + pagesize);
		
		val posts: List<Post> = session.run(queryOf("""
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
							    mapOf("pageId" to page, "pageSize" to pagesize, "show_hidden" to false))
							.map { row ->
							    Post( row.int("post_id")
								, row.string("title")
								, row.string("content")
								, row.int("creator_id")
								, listOf("test", "tags") // row.string("tags")
								, row.localDateTime("created_at"))
							}
							.asList
		)

		println("Posts: " + posts) 

		call.respond(posts);
	    }
	}    
    }	
}

/**
 * Makes a connection to a Postgres database.
 *
 * In order to connect to your running Postgres process,
 * please specify the following parameters in your configuration file:
 * - postgres.url -- Url of your running database process.
 * - postgres.user -- Username for database connection
 * - postgres.password -- Password for database connection
 *
 * If you don't have a database process running yet, you may need to [download]((https://www.postgresql.org/download/))
 * and install Postgres and follow the instructions [here](https://postgresapp.com/).
 * Then, you would be able to edit your url,  which is usually "jdbc:postgresql://host:port/database", as well as
 * user and password values.
 *
 *
 * @param embedded -- if [true] defaults to an embedded database for tests that runs locally in the same process.
 * In this case you don't have to provide any parameters in configuration file, and you don't have to run a process.
 *
 * @return [Connection] that represent connection to the database. Please, don't forget to close this connection when
 * your application shuts down by calling [Connection.close]
 * */
fun Application.connectToPostgres(): Connection {
    Class.forName("org.postgresql.Driver")
    val url = environment.config.property("postgres.url").getString()
    val user = environment.config.property("postgres.user").getString()
    val password = environment.config.property("postgres.password").getString()

    return DriverManager.getConnection(url, user, password)
}

