/**
 * @file   react_utils.cpp
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 15:25:04
 * 
 * @brief  
 * 
 * 
 */

#include "react_utils.h"
#include "react/react.hpp"
#include "zmalloc.h"
#include "memory.h"
#include "common.h"

using namespace react;

typedef struct react_ctx_t {
    void *aggregator;
} react_ctx_t;

class file_aggregator_t : public aggregator_t {
    public:
        file_aggregator_t(const char *filename){
            hfile = open(filename, O_CREAT | O_TRUNC | O_WRONLY, 0640);
        }
        ~file_aggregator_t() {
            close(hfile);
        }
        void aggregate(const call_tree_t &call_tree) {
            std::string strCallTree = print_json_to_string(call_tree);
            ::write(hfile, strCallTree.c_str(), strCallTree.length());
            ::sync();
        }

    private:
        int hfile;
};

void *react_init(const char *react_log_filename)
{
    react_ctx_t *react_ctx = (react_ctx_t*)zmalloc(sizeof(react_ctx_t));
    memset(react_ctx, 0, sizeof(react_ctx_t));

    react_ctx->aggregator = new file_aggregator_t(react_log_filename);
    react_activate(react_ctx->aggregator);

    return react_ctx;
}

void react_cleanup(void *opaque)
{
    react_deactivate();

    react_ctx_t *react_ctx = (react_ctx_t*)opaque;
    delete (aggregator_t*)react_ctx->aggregator;

    zfree(react_ctx);
}

 
