/**
 * @file   react_utils.,h
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   22014-10-10 15:41:04
 * 
 * @brief  
 * 
 * 
 */
#ifndef __REACT_UTILS_H__
#define __REACT_UTILS_H__

#include "react/react.h"

#ifndef Q_EXTERN_C
#  ifdef __cplusplus
#    define Q_EXTERN_C extern "C"
#  else
#    define Q_EXTERN_C extern
#  endif
#endif

Q_EXTERN_C void *react_init(const char *react_log_filename);
Q_EXTERN_C void react_cleanup(void* react_ctx);

#define REACT_INIT(log_filename) react_init(log_filename)

#define REACT_CLEANUP(react_ctx) react_cleanup(react_ctx)

#define REACT_ACTION_START(actionString) \
        const int __react_id_##actionString = react_define_new_action(#actionString); \
        react_start_action(__react_id_##actionString); \


#define REACT_ACTION_STOP(actionString) \
        react_stop_action(__react_id_##actionString); \

#define REACT_ACTIVATE(aggregator) \
    react_activate(aggregator);

#define REACT_DEACTIVATE() \
    react_deactivate();

#define REACT_CREATE_SUBTHREAD_AGGREGATOR() \
    react_create_subthread_aggregator();

#define REACT_DESTROY_SUBTHREAD_AGGREGATOR(aggregator) \
    react_destroy_subthread_aggregator(aggregator);


#endif /* __REACT_UTILS_H__ */

