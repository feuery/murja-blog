# PostsApi

All URIs are relative to **

Method | HTTP request | Description
------------- | ------------- | -------------
[**apiPostsAllLimitGet**](PostsApi.md#apiPostsAllLimitGet) | **GET** /api/posts/all/{limit} | Returns first $limit of all posts sorted by their creation date DESC
[**apiPostsAllTitlesGet**](PostsApi.md#apiPostsAllTitlesGet) | **GET** /api/posts/all-titles | Same as /titles, but auths that requester has edit-post - permission
[**apiPostsCommentIdDelete**](PostsApi.md#apiPostsCommentIdDelete) | **DELETE** /api/posts/comment/{id} | Deletes a comment and returns its id
[**apiPostsExistingLandingPageGet**](PostsApi.md#apiPostsExistingLandingPageGet) | **GET** /api/posts/existing-landing-page | Returns either an empty string or the title of already existing landing page
[**apiPostsPagePagePageSizePageSizeGet**](PostsApi.md#apiPostsPagePagePageSizePageSizeGet) | **GET** /api/posts/page/{page}/page-size/{page-size} | Returns a page of specific size. Posts are sorted by their creation date DESC
[**apiPostsPostIdAllowHiddenAllowHiddenGet**](PostsApi.md#apiPostsPostIdAllowHiddenAllowHiddenGet) | **GET** /api/posts/post/{id}/allow-hidden/{allow-hidden} | Returns a post per its id. Can return also hidden posts if edit-post permission is held
[**apiPostsPostIdCommentPost**](PostsApi.md#apiPostsPostIdCommentPost) | **POST** /api/posts/post/{id}/comment | Comments a post and returns it with the new comment appended
[**apiPostsPostIdDelete**](PostsApi.md#apiPostsPostIdDelete) | **DELETE** /api/posts/post/{id} | Deletes a post and returns its id
[**apiPostsPostIdGet**](PostsApi.md#apiPostsPostIdGet) | **GET** /api/posts/post/{id} | Returns a post per its id
[**apiPostsPostIdVersionVersionDelete**](PostsApi.md#apiPostsPostIdVersionVersionDelete) | **DELETE** /api/posts/post/{id}/version/{version} | Deletes a version of a post
[**apiPostsPostIdVersionVersionGet**](PostsApi.md#apiPostsPostIdVersionVersionGet) | **GET** /api/posts/post/{id}/version/{version} | Returns an old version of the post and the current comments
[**apiPostsPostIdVersionsGet**](PostsApi.md#apiPostsPostIdVersionsGet) | **GET** /api/posts/post/{id}/versions | 
[**apiPostsPostPost**](PostsApi.md#apiPostsPostPost) | **POST** /api/posts/post | Writes a new post into the db
[**apiPostsPostPut**](PostsApi.md#apiPostsPostPut) | **PUT** /api/posts/post | Edits a post
[**apiPostsTitlesGet**](PostsApi.md#apiPostsTitlesGet) | **GET** /api/posts/titles | Returns titles, tags, months and years for the title-widget


## **apiPostsAllLimitGet**

Returns first $limit of all posts sorted by their creation date DESC

### Example
```bash
 apiPostsAllLimitGet limit=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **limit** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsAllTitlesGet**

Same as /titles, but auths that requester has edit-post - permission

### Example
```bash
 apiPostsAllTitlesGet
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**array[Murja.specs.timedtitleTimedTitle]**](Murja.specs.timedtitleTimedTitle.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsCommentIdDelete**

Deletes a comment and returns its id

### Example
```bash
 apiPostsCommentIdDelete id=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsExistingLandingPageGet**

Returns either an empty string or the title of already existing landing page

### Example
```bash
 apiPostsExistingLandingPageGet
```

### Parameters
This endpoint does not need any parameter.

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPagePagePageSizePageSizeGet**

Returns a page of specific size. Posts are sorted by their creation date DESC

### Example
```bash
 apiPostsPagePagePageSizePageSizeGet page=value page-size=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **page** | **integer** |  |
 **pageSize** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdAllowHiddenAllowHiddenGet**

Returns a post per its id. Can return also hidden posts if edit-post permission is held

### Example
```bash
 apiPostsPostIdAllowHiddenAllowHiddenGet id=value allow-hidden=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |
 **allowHidden** | **boolean** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdCommentPost**

Comments a post and returns it with the new comment appended

### Example
```bash
 apiPostsPostIdCommentPost id=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |
 **murja.specs.post/NewComment** | [**Murja.specs.postNewcomment**](Murja.specs.postNewcomment.md) |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdDelete**

Deletes a post and returns its id

### Example
```bash
 apiPostsPostIdDelete id=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdGet**

Returns a post per its id

### Example
```bash
 apiPostsPostIdGet id=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdVersionVersionDelete**

Deletes a version of a post

### Example
```bash
 apiPostsPostIdVersionVersionDelete id=value version=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |
 **version** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdVersionVersionGet**

Returns an old version of the post and the current comments

### Example
```bash
 apiPostsPostIdVersionVersionGet id=value version=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |
 **version** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostIdVersionsGet**



### Example
```bash
 apiPostsPostIdVersionsGet id=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **integer** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostPost**

Writes a new post into the db

### Example
```bash
 apiPostsPostPost
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **murja.specs.post/NewPost** | [**Murja.specs.postNewpost**](Murja.specs.postNewpost.md) |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsPostPut**

Edits a post

### Example
```bash
 apiPostsPostPut
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **murja.specs.post/EditedPost** | [**Murja.specs.postEditedpost**](Murja.specs.postEditedpost.md) |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPostsTitlesGet**

Returns titles, tags, months and years for the title-widget

### Example
```bash
 apiPostsTitlesGet
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**array[Murja.specs.timedtitleTimedTitle]**](Murja.specs.timedtitleTimedTitle.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

