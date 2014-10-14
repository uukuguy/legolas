/**
 * @file  test_main.cpp
 * @author Jiangwen Su <uukuguy@gmail.com>
 * @date   2014-10-10 09:41:32
 * 
 * @brief  
 * 
 * 
 */

#include <gtest/gtest.h>

#include "function.h"

class FunctionTest : public testing::Test{
    protected:

        static void SetUpTestCase(){
        }

        static void TearDownTestCase(){
        }

        virtual void SetUp(){
        }

        virtual void TearDown(){
        }

};

TEST_F(FunctionTest, from_function_test)
{
    
    function_t *func_A = function_new(1, "Func_A", 6);
    function_t *func_A1 = function_new(11, "Func_A1", 7);
    function_t *func_A2 = function_new(12, "Func_A2", 7);
    function_t *func_A3 = function_new(13, "Func_A3", 7);

    function_t *func_S = function_new(0, "Func_S", 6);


    EXPECT_TRUE(function_add_from_function_if_not_exist(func_A, func_A1) == 0);
    EXPECT_TRUE(function_add_from_function_if_not_exist(func_A, func_A1) == -1);
    EXPECT_EQ(func_A->from_functions_count, 1);

    EXPECT_TRUE(function_add_from_function_if_not_exist(func_A, func_A2) == 0);
    EXPECT_TRUE(function_add_from_function_if_not_exist(func_A, func_A2) == -1);
    EXPECT_EQ(func_A->from_functions_count, 2);

    EXPECT_TRUE(function_add_from_function_if_not_exist(func_A, func_A3) == 0);
    EXPECT_TRUE(function_add_from_function_if_not_exist(func_A, func_A3) == -1);
    EXPECT_EQ(func_A->from_functions_count, 3);

    EXPECT_TRUE(function_find_from_function(func_A, func_A1) == 2); 
    EXPECT_TRUE(function_find_from_function(func_A, func_A2) == 1); 
    EXPECT_TRUE(function_find_from_function(func_A, func_A3) == 0); 

    EXPECT_TRUE(function_find_from_function(func_A, func_S) == -1); 

    function_free(func_A);
    function_free(func_A1);
    function_free(func_A2);
    function_free(func_A3);
    function_free(func_S);
}

TEST_F(FunctionTest, to_function_test)
{
    
    function_t *func_A = function_new(1, "Func_A", 6);
    function_t *func_A1 = function_new(11, "Func_A1", 7);
    function_t *func_A2 = function_new(12, "Func_A2", 7);
    function_t *func_A3 = function_new(13, "Func_A3", 7);

    function_t *func_S = function_new(0, "Func_S", 6);


    EXPECT_TRUE(function_add_to_function_if_not_exist(func_A, func_A1) == 0);
    EXPECT_TRUE(function_add_to_function_if_not_exist(func_A, func_A1) == -1);
    EXPECT_EQ(func_A->to_functions_count, 1);

    EXPECT_TRUE(function_add_to_function_if_not_exist(func_A, func_A2) == 0);
    EXPECT_TRUE(function_add_to_function_if_not_exist(func_A, func_A2) == -1);
    EXPECT_EQ(func_A->to_functions_count, 2);

    EXPECT_TRUE(function_add_to_function_if_not_exist(func_A, func_A3) == 0);
    EXPECT_TRUE(function_add_to_function_if_not_exist(func_A, func_A3) == -1);
    EXPECT_EQ(func_A->to_functions_count, 3);

    EXPECT_TRUE(function_find_to_function(func_A, func_A1) == 2); 
    EXPECT_TRUE(function_find_to_function(func_A, func_A2) == 1); 
    EXPECT_TRUE(function_find_to_function(func_A, func_A3) == 0); 

    EXPECT_TRUE(function_find_to_function(func_A, func_S) == -1); 

    function_free(func_A);
    function_free(func_A1);
    function_free(func_A2);
    function_free(func_A3);
    function_free(func_S);
}

class TestEnvironment : public testing::Environment{
    public:
        virtual void SetUp() {
        }

        virtual void TearDown() {
        }
};

int main(int argc, char *argv[])
{
    testing::AddGlobalTestEnvironment(new TestEnvironment);
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

