ALTER TABLE blog.post ADD CONSTRAINT not_hidden_and_unlisted CHECK ((tags ?? 'hidden' AND NOT tags ?? 'unlisted') OR
      	    	      	  	     			     	    (NOT tags ?? 'hidden' AND tags ?? 'unlisted') OR
								    (NOT tags ?? 'hidden' AND NOT tags ?? 'unlisted'))
