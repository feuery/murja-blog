CREATE OR REPLACE FUNCTION push_to_history()
RETURNS TRIGGER
LANGUAGE plpgsql
AS
$$
DECLARE local_version INT;
BEGIN
	SELECT  coalesce(MAX(ph.version), 0) + 1 INTO local_version 
	FROM blog.Post p
	LEFT JOIN blog.Post_History ph ON p.ID = ph.ID
	WHERE p.id = OLD.id
	GROUP BY p.ID;

	INSERT INTO blog.Post_History(ID, Title, Content, creator_id, tags, created_at, version)
	VALUES (OLD.ID, OLD.Title, OLD.Content, OLD.creator_id, OLD.tags, OLD.created_at, local_version);

	RETURN NEW;
END;
$$;

CREATE TRIGGER history_pushing
BEFORE UPDATE
ON blog.Post
FOR EACH ROW
EXECUTE PROCEDURE push_to_history();
