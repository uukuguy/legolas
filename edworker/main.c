/**
 * @file   main.c
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-11-08 01:23:17
 * 
 * @brief  
 * 
 * 
 */

#include <czmq.h>
#include "common.h"

const char *edbroker_backend_endpoint = "tcp://127.0.0.1:19978";

extern int run_worker(void);

int main(int argc, char *argv[])
{
    return run_worker();
}

