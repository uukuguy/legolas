#include "CService.h"
typedef unsigned long DWORD;
#ifdef _CURL_
int Service::HttpRequest( const string http_method,
	const string url )
{
	vector<string> custom_headers = vector<string>();
	PostData post_data;
	return HttpRequest(http_method, url, custom_headers, post_data,this);
}

int Service::HttpRequest( const string http_method,
	const string url,
	const PostData& post_data )
{
	vector<string> custom_headers = vector<string>();
	return HttpRequest(http_method, url, custom_headers, post_data, this);
}


//////////////////////////////////////////////////////////////////////////////// 
// Private members 
//////////////////////////////////////////////////////////////////////////////// 

Service::Service()
{
	m_lpPutData = NULL;
	m_pGetDownLoadProcess = NULL;
	m_pGetProcess = NULL;
	m_pFileWriteStruct = NULL;
	m_pFileAccessStruct = NULL;
}

size_t Service::WriteCallback( char* data, size_t size, size_t nitems, void* userp )
{
	size_t	result = 0; 
	if ( userp )
	{
		Service* lpService = (Service*)userp;
		if (lpService->m_pFileWriteStruct)
		{
			lpService->m_pFileWriteStruct->WriteBlock( lpService->m_pFileWriteStruct,data,size * nitems );
		}
		else
		{
			lpService->m_resp_buffer.append(data, size * nitems); 
		}
		result = size * nitems;  // How much did we write? 
	}
	return result ;
}

size_t Service::WriteHeaderCallback( char *data, size_t size, size_t nmemb, void* userp )
{
	size_t result = 0; 
	if ( userp )
	{
		Service* lpService = (Service*)userp;

		lpService->m_resp_header.append(data, size * nmemb); 
		result = size * nmemb;  // How much did we write? 
	} 
	return result; 
}


size_t Service::UploadCallback( void *buffer, size_t sz, size_t nmemb, void *userp )
{
	size_t rd = 0;
	if ( userp )
	{
		Service *lpService = (Service*)userp;

		if ( lpService->m_lpPutData )
		{
			 PutData* lpPut = (PutData*)lpService->m_lpPutData;
			rd = (nmemb*sz > lpPut->datalen)?lpPut->datalen:(nmemb*sz);
			memcpy(buffer, lpPut->data, rd);
			if (lpService->m_pGetProcess && lpService->m_pGetProcess->GetProcessFunc)
			{
				lpService->m_pGetProcess->GetProcessFunc(lpService->m_pGetProcess,
					((DWORD)lpPut->postion+rd)*100.0/(lpPut->totallen!=0?lpPut->totallen:lpPut->datalen));
			}

			char szbuf[MAX_PATH]={0};
			sprintf(szbuf,"postion=%ld,%lf%%",(DWORD)lpPut->postion+rd,((DWORD)lpPut->postion+rd)*100.0/lpPut->totallen!=0?lpPut->totallen:lpPut->datalen);
			//OutputDebugStringA(szbuf);
			//OutputDebugStringA("\r\n");
			lpPut->data += rd;
			lpPut->datalen -= rd;
			lpPut->postion += rd;

			return  rd;
		}
	}
	return 0;
}




int Service::ProgressCallback( const void* userp, double dltotal, double dlnow, double ultotal, double ulnow )
{
	if ( userp )
	{
		Service* lpService = (Service*)userp;
		if( lpService->m_pGetDownLoadProcess && lpService->m_pGetDownLoadProcess->GetProcessFunc )
		{
			if ( dltotal )
			{
				lpService->m_pGetDownLoadProcess->GetProcessFunc(lpService->m_pGetDownLoadProcess,dlnow*100.0/dltotal);
			}
		}
	}
	return 0;
}

int Service::HttpRequest( const string http_method,
	const string url,
	const vector<string>& custom_headers,
	const PostData& post_data,
	Service* lpService)
{
	//	string resp_buffer;  // body of the response from the server
	char *memblock = NULL;  // file size of POST body
	CURL *curl = curl_easy_init();
	//const char *progress_data = "* ";
	if ( curl ) 
	{
		char errorBuffer[CURL_ERROR_SIZE]={0};
		//curl_easy_setopt(curl, CURLOPT_PROXY, "10.142.49.127:8088");
		curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
		curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
		curl_easy_setopt(curl, CURLOPT_HEADER, 0);
		curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);
		curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
		curl_easy_setopt(curl, CURLOPT_WRITEDATA, lpService);
		curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
		curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);
		curl_easy_setopt(curl, CURLOPT_VERBOSE , 1);

		//下载进度条
		curl_easy_setopt(curl, CURLOPT_NOPROGRESS, FALSE);
		curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, ProgressCallback);
		curl_easy_setopt(curl, CURLOPT_PROGRESSDATA, lpService);


		//curl_easy_setopt(curl,CURLOPT_SSLVERSION,3);

		struct curl_slist *headers = NULL;

		// Add standard headers
		for (unsigned int i = 0; i < request_headers_.size(); ++i) {
			headers = curl_slist_append(headers, request_headers_[i].c_str());
		}

		// Add any custom headers
		for (unsigned int i = 0; i < custom_headers.size(); ++i) {
			headers = curl_slist_append(headers, custom_headers[i].c_str());
		}

		if ( http_method == "GET" )
		{
			curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L);
			headers =
				curl_slist_append(headers, "Content-Type: application/atom+xml");

		} 
		else if ( http_method == "POST" )
		{
			curl_easy_setopt(curl, CURLOPT_POST, 1);
			// Configure post for plain text or binary data
			if ( post_data.data != NULL )
			{
				curl_easy_setopt(curl, CURLOPT_POSTFIELDS, post_data.data);
				curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, strlen(post_data.data));
			}
			else
			{

			}
		}
		else if ( http_method == "PUT" )
		{
			curl_easy_setopt(curl, CURLOPT_PUT, 1L);

		} 
		else if ( http_method == "DELETE" )
		{
			curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");

		}
		else
		{
//			cerr << "Error: Unknown http method - " << http_method << endl;
			error_str = "Error: Unknown http method - " + http_method;
			//return AVERR_HTTP_METHOD;
			return CURLE_NOT_BUILT_IN;
		}

		// attach headers to this request
		curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

		CURLcode curl_code = curl_easy_perform(curl);

		int http_code = 0;
		curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

		// clean up
		curl_easy_cleanup(curl);
		curl_slist_free_all(headers);
		if ( memblock != NULL )
		{
			delete[] memblock;
		}
		
		if ( curl_code != CURLE_OK ) 
		{

//			cout << "\nError: [" << curl_code << "] - " << errorBuffer;
			//DebugPrintA(2, "\nError--%s\n",errorBuffer);
			error_str = errorBuffer;
			return curl_code;

		}
		else if ( 400 <= http_code )
		{
			int start, end;
			start = lpService->m_resp_buffer.find("<internalReason>");
			end = lpService->m_resp_buffer.find("</internalReason>");
			if( start>=0 && end>=0 && end>start )
			{
				start += 16;
				error_str = lpService->m_resp_buffer.substr(start, end-start);
			}
			else
				error_str = lpService->m_resp_buffer;

			return http_code;
		}
		else
			error_str = "";
	}

	return CURLE_OK;
}



int Service::HttpRequest( const string http_method,
	const string url,
	const vector<string>& custom_headers,
	const PutData* lpPutData,
	Service* lpService)
{
	//	string resp_buffer;  // body of the response from the server
	char *memblock = NULL;  // file size of POST body
	curl_global_init(CURL_GLOBAL_ALL);
	CURL *curl = curl_easy_init();
	char errorBuffer[CURL_ERROR_SIZE]={0};
	if ( curl ) 
	{
		//curl_easy_setopt(curl, CURLOPT_PROXY, "10.142.49.127:8088");
		curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, errorBuffer);
		curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
		curl_easy_setopt(curl, CURLOPT_HEADER, 0);
		curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1);
		curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
		curl_easy_setopt(curl, CURLOPT_WRITEDATA, lpService);
		curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION,WriteHeaderCallback); // our static function
		curl_easy_setopt(curl, CURLOPT_WRITEHEADER, lpService); //"headers" is a member variable referencing HttpHeaders 
		curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
		curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);
		curl_easy_setopt(curl, CURLOPT_VERBOSE , 1);

		//下载进度条
		curl_easy_setopt(curl, CURLOPT_NOPROGRESS, FALSE);
		curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, ProgressCallback);
		curl_easy_setopt(curl, CURLOPT_PROGRESSDATA, lpService);

		//curl_easy_setopt(curl,CURLOPT_SSLVERSION,3);

		struct curl_slist *headers = NULL;
		// Add standard headers
		for (unsigned int i = 0; i < request_headers_.size(); ++i) 
		{
			headers = curl_slist_append(headers, request_headers_[i].c_str());
		}

		// Add any custom headers
		for (unsigned int i = 0; i < custom_headers.size(); ++i) 
		{
			headers = curl_slist_append(headers, custom_headers[i].c_str());
		}

		if ( http_method == "GET" )
		{
			curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L);
			headers =
				curl_slist_append(headers, "Content-Type: application/atom+xml");

		}
		else if ( http_method == "POST" ) 
		{
			curl_easy_setopt(curl, CURLOPT_POST, 1);
			if(lpPutData != NULL && lpPutData->data != NULL && lpPutData->datalen>0)
			{
				//  curl_easy_setopt(curl, CURLOPT_BINARYTRANSFER, 1);
				curl_easy_setopt(curl, CURLOPT_POSTFIELDS, lpPutData->data);
				curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, lpPutData->datalen);
			}
		}
		else if ( http_method == "PUT" )
		{
			curl_easy_setopt(curl, CURLOPT_PUT, 1L);

			if( lpPutData != NULL && lpPutData->data != NULL )
			{
				curl_easy_setopt(curl, CURLOPT_UPLOAD, 1L);

				/* we want to use our own read function */
				curl_easy_setopt(curl, CURLOPT_READFUNCTION, UploadCallback);
				lpService->m_lpPutData = lpPutData;
				/* now specify which file to upload */
				/*curl_easy_setopt(curl, CURLOPT_READDATA, lpPutData);
				curl_easy_setopt(curl, CURLOPT_INFILESIZE, lpPutData->datalen);*/
				curl_easy_setopt(curl, CURLOPT_READDATA, lpService);
				curl_easy_setopt(curl, CURLOPT_INFILESIZE, lpService->m_lpPutData->datalen);
			}

		} 
		else if ( http_method == "DELETE" )
		{
			curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");

		} 
		else 
		{
			//cerr << "Error: Unknown http method - " << http_method << endl;

			error_str = "Error: Unknown http method - " + http_method;
			//return AVERR_HTTP_METHOD;
			return CURLE_NOT_BUILT_IN;
		}

		// attach headers to this request
		curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

		CURLcode curl_code = curl_easy_perform(curl);

		int http_code = 0;
		curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &http_code);

		// clean up
		curl_easy_cleanup(curl);
		curl_slist_free_all(headers);
		curl_global_cleanup();
		if ( memblock != NULL )
		{
			delete[] memblock;
		}
		
		if ( curl_code != CURLE_OK )
		{

			//cout << "\nError: [" << curl_code << "] - " << errorBuffer;
			//DebugPrintA(2, "\nError--%s\n",errorBuffer);
			error_str = errorBuffer;
			//linux modify callback error info????
			//::MessageBoxA(NULL,error_str.c_str(),"HttpReq-ER",0);
			return curl_code;

		}
		else if ( 400 <= http_code )
		{
			//DebugPrintA(3, "Http-code=%d",http_code);
			return http_code;
		}
		else
			error_str = "";
	}
	return CURLE_OK;
}


void Service::SetUploadProcess( GETPROCESS* pGetProcess )
{
    m_pGetProcess = pGetProcess;
}

void Service::SetDownloaddProcess( GETPROCESS* pGetDownLoadProcess )
{
    m_pGetDownLoadProcess = pGetDownLoadProcess;
}

void Service::SetFileWrite( FILEWRITE* pFileWriteStruct )
{
	m_pFileWriteStruct = pFileWriteStruct;
}
#endif

