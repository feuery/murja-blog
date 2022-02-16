#  Bash client

## Overview
This is a Bash client script for accessing  service.

The script uses cURL underneath for making all REST calls.

## Usage

```shell
# Make sure the script has executable rights
$ chmod u+x 

# Print the list of operations available on the service
$ ./ -h

# Print the service description
$ ./ --about

# Print detailed information about specific operation
$ ./ <operationId> -h

# Make GET request
./ --host http://<hostname>:<port> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make GET request using arbitrary curl options (must be passed before <operationId>) to an SSL service using username:password
 -k -sS --tlsv1.2 --host https://<hostname> -u <user>:<password> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make POST request
$ echo '<body_content>' |  --host <hostname> --content-type json <operationId> -

# Make POST request with simple JSON content, e.g.:
# {
#   "key1": "value1",
#   "key2": "value2",
#   "key3": 23
# }
$ echo '<body_content>' |  --host <hostname> --content-type json <operationId> key1==value1 key2=value2 key3:=23 -

# Preview the cURL command without actually executing it
$  --host http://<hostname>:<port> --dry-run <operationid>

```

## Docker image
You can easily create a Docker image containing a preconfigured environment
for using the REST Bash client including working autocompletion and short
welcome message with basic instructions, using the generated Dockerfile:

```shell
docker build -t my-rest-client .
docker run -it my-rest-client
```

By default you will be logged into a Zsh environment which has much more
advanced auto completion, but you can switch to Bash, where basic autocompletion
is also available.

## Shell completion

### Bash
The generated bash-completion script can be either directly loaded to the current Bash session using:

```shell
source .bash-completion
```

Alternatively, the script can be copied to the `/etc/bash-completion.d` (or on OSX with Homebrew to `/usr/local/etc/bash-completion.d`):

```shell
sudo cp .bash-completion /etc/bash-completion.d/
```

#### OS X
On OSX you might need to install bash-completion using Homebrew:
```shell
brew install bash-completion
```
and add the following to the `~/.bashrc`:

```shell
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
```

### Zsh
In Zsh, the generated `_` Zsh completion file must be copied to one of the folders under `$FPATH` variable.


## Documentation for API Endpoints

All URIs are relative to **

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*LoginApi* | [**apiLoginLoginPost**](docs/LoginApi.md#apiloginloginpost) | **POST** /api/login/login | 
*LoginApi* | [**apiLoginLogoutPost**](docs/LoginApi.md#apiloginlogoutpost) | **POST** /api/login/logout | 
*LoginApi* | [**apiLoginSessionGet**](docs/LoginApi.md#apiloginsessionget) | **GET** /api/login/session | 
*MediaApi* | [**apiPicturesIdGet**](docs/MediaApi.md#apipicturesidget) | **GET** /api/pictures/{id} | 
*MediaApi* | [**apiPicturesListAllGet**](docs/MediaApi.md#apipictureslistallget) | **GET** /api/pictures/list/all | 
*MediaApi* | [**apiPicturesPost**](docs/MediaApi.md#apipicturespost) | **POST** /api/pictures | 
*PostsApi* | [**apiPostsAllLimitGet**](docs/PostsApi.md#apipostsalllimitget) | **GET** /api/posts/all/{limit} | Returns first $limit of all posts sorted by their creation date DESC
*PostsApi* | [**apiPostsAllTitlesGet**](docs/PostsApi.md#apipostsalltitlesget) | **GET** /api/posts/all-titles | Same as /titles, but auths that requester has edit-post - permission
*PostsApi* | [**apiPostsCommentIdDelete**](docs/PostsApi.md#apipostscommentiddelete) | **DELETE** /api/posts/comment/{id} | Deletes a comment and returns its id
*PostsApi* | [**apiPostsExistingLandingPageGet**](docs/PostsApi.md#apipostsexistinglandingpageget) | **GET** /api/posts/existing-landing-page | Returns either an empty string or the title of already existing landing page
*PostsApi* | [**apiPostsPagePagePageSizePageSizeGet**](docs/PostsApi.md#apipostspagepagepagesizepagesizeget) | **GET** /api/posts/page/{page}/page-size/{page-size} | Returns a page of specific size. Posts are sorted by their creation date DESC
*PostsApi* | [**apiPostsPostIdAllowHiddenAllowHiddenGet**](docs/PostsApi.md#apipostspostidallowhiddenallowhiddenget) | **GET** /api/posts/post/{id}/allow-hidden/{allow-hidden} | Returns a post per its id. Can return also hidden posts if edit-post permission is held
*PostsApi* | [**apiPostsPostIdCommentPost**](docs/PostsApi.md#apipostspostidcommentpost) | **POST** /api/posts/post/{id}/comment | Comments a post and returns it with the new comment appended
*PostsApi* | [**apiPostsPostIdDelete**](docs/PostsApi.md#apipostspostiddelete) | **DELETE** /api/posts/post/{id} | Deletes a post and returns its id
*PostsApi* | [**apiPostsPostIdGet**](docs/PostsApi.md#apipostspostidget) | **GET** /api/posts/post/{id} | Returns a post per its id
*PostsApi* | [**apiPostsPostIdVersionVersionDelete**](docs/PostsApi.md#apipostspostidversionversiondelete) | **DELETE** /api/posts/post/{id}/version/{version} | Deletes a version of a post
*PostsApi* | [**apiPostsPostIdVersionVersionGet**](docs/PostsApi.md#apipostspostidversionversionget) | **GET** /api/posts/post/{id}/version/{version} | Returns an old version of the post and the current comments
*PostsApi* | [**apiPostsPostIdVersionsGet**](docs/PostsApi.md#apipostspostidversionsget) | **GET** /api/posts/post/{id}/versions | 
*PostsApi* | [**apiPostsPostPost**](docs/PostsApi.md#apipostspostpost) | **POST** /api/posts/post | Writes a new post into the db
*PostsApi* | [**apiPostsPostPut**](docs/PostsApi.md#apipostspostput) | **PUT** /api/posts/post | Edits a post
*PostsApi* | [**apiPostsTitlesGet**](docs/PostsApi.md#apipoststitlesget) | **GET** /api/posts/titles | Returns titles, tags, months and years for the title-widget
*SettingsApi* | [**apiSettingsClientSettingsGet**](docs/SettingsApi.md#apisettingsclientsettingsget) | **GET** /api/settings/client-settings | 
*UsersApi* | [**apiUsersIsEmptyGet**](docs/UsersApi.md#apiusersisemptyget) | **GET** /api/users/is-empty | 
*UsersApi* | [**apiUsersSavePost**](docs/UsersApi.md#apiuserssavepost) | **POST** /api/users/save | 


## Documentation For Models

 - [Murja.specs.loginlogin](docs/Murja.specs.loginlogin.md)
 - [Murja.specs.postEditedpost](docs/Murja.specs.postEditedpost.md)
 - [Murja.specs.postNewcomment](docs/Murja.specs.postNewcomment.md)
 - [Murja.specs.postNewpost](docs/Murja.specs.postNewpost.md)
 - [Murja.specs.timedtitleTimedTitle](docs/Murja.specs.timedtitleTimedTitle.md)
 - [Murja.specs.updateduserupdateduser](docs/Murja.specs.updateduserupdateduser.md)


## Documentation For Authorization

 All endpoints do not require authorization.

