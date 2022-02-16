# MediaApi

All URIs are relative to **

Method | HTTP request | Description
------------- | ------------- | -------------
[**apiPicturesIdGet**](MediaApi.md#apiPicturesIdGet) | **GET** /api/pictures/{id} | 
[**apiPicturesListAllGet**](MediaApi.md#apiPicturesListAllGet) | **GET** /api/pictures/list/all | 
[**apiPicturesPost**](MediaApi.md#apiPicturesPost) | **POST** /api/pictures | 


## **apiPicturesIdGet**



### Example
```bash
 apiPicturesIdGet id=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **string** |  |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, application/transit+msgpack, application/transit+json, application/edn
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **apiPicturesListAllGet**



### Example
```bash
 apiPicturesListAllGet
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

## **apiPicturesPost**



### Example
```bash
 apiPicturesPost
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file** | **File** |  | [optional]

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json, application/transit+msgpack, application/transit+json, application/edn

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

