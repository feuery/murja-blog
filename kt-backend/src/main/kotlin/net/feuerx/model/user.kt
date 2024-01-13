package net.feuerx.model

import kotlinx.serialization.Serializable
import kotlinx.serialization.SerialName
import kotliquery.queryOf
import kotliquery.Session

@Serializable
data class User( val username: String
	       , val nickname: String
	       , val img_location: String)


data class IntermediateryUser( val username: String
		      , val nickname: String
		      , val img_location: String
		      , val primary_group_name: String
		      , val primarygroup: Boolean
		      , val userid: Long
		      , val action: String)

@Serializable
data class SessionUser( val username: String
		      , val nickname: String
		      , val img_location: String
		      , @SerialName("primary-group-name")
			val primary_group_name: String
		      , val primarygroup: Boolean
		      , val userid: Long
		      , val permissions: List<String>)
			
			

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

fun getSessionUserById(session: Session, id: Long): SessionUser {
    val users = session.run(queryOf(
                                """
SELECT u.Username, u.Nickname, u.Img_location, ug.Name as "primary-group-name", gm.PrimaryGroup, u.ID as userid, perm.action
FROM blog.Users u
JOIN blog.GroupMapping gm ON u.ID = gm.UserID
JOIN blog.UserGroup ug ON ug.ID = gm.GroupID
JOIN blog.grouppermissions gp ON gp.groupid = gm.groupid
JOIN blog.permission perm ON perm.id = gp.permissionid
WHERE u.ID = :id""",
                                mapOf("id" to id)
                       )
                           .map { row ->
                               IntermediateryUser( row.string("username")
                                          , row.string("nickname")
                                          , row.string("img_location")
					  , row.string("primary-group-name")
					  , row.boolean("PrimaryGroup")
					  , row.long("userid")
					  , row.string("action"))
                                }
                           .asList)
    val demouser = users.first()
    val transformed_user = SessionUser( demouser.username
				      , demouser.nickname
				      , demouser.img_location
				      , demouser.primary_group_name
				      , demouser.primarygroup
				      , demouser.userid
				      , users.map { user -> user.action})
    return transformed_user
}
