package net.feuerx.model

import kotlinx.serialization.Serializable
import kotliquery.queryOf
import kotliquery.Session

@Serializable
data class User( val username: String
	       , val nickname: String
	       , val img_location: String)

fun getUserById (session: Session, id: Long): User {
    return session.run(queryOf("""
			    SELECT username, nickname, img_location
			    FROM blog.Users
			    WHERE id = :id""",
			       mapOf("id" to id))
			   .map { row ->
			       User(row.string("username"), row.string("nickname"), row.string("img_location"))
			   }
			   .asList).first()
}
