/**
 * @file   main.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-08 01:14:11
 * 
 * @brief  
 * 
 * 
 */

#include "common.h"

const char *edbroker_frontend_endpoint = "tcp://127.0.0.1:19977";

extern int run_client(void);

int main(int arc, char *argv[])
{
    int msec0, msec1;

    /* -------- Begin Timing -------- */
    struct timeval tv; 
    gettimeofday(&tv, NULL); 
    msec0 = tv.tv_sec * 1000 + tv.tv_usec / 1000; 

    int rc = run_client();

    /* -------- End Timing -------- */
    gettimeofday(&tv, NULL); 
    msec1 = tv.tv_sec * 1000 + tv.tv_usec / 1000; 

    int msec = msec1 - msec0;
    printf("========> Total Time: %d.%03d sec.<========\n", msec / 1000, msec % 1000);
    printf("~~> End EverData Client.\n");

    return rc;
}

