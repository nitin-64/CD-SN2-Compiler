#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>

#define HASH_TABLE_SIZE 50

/* struct to hold each entry in symbol table */

struct entry_st
{
    char* lexeme;
    int token_name;
    struct entry_st* next;
};

typedef struct entry_st entry_ht;


// function to create a new hash table

entry_ht** create_new_hash_table() 
{

    entry_ht** hash_table_pointer = NULL; // declaration of pointer

    hash_table_pointer = malloc(sizeof(entry_ht*) * HASH_TABLE_SIZE);  // allocate memory to hash table array of size HASH_TABLE_SIZE

    // initalizing all the pointers in the array as null

    int i;

    for (i = 0; i < HASH_TABLE_SIZE; ++i) 
    {
        hash_table_pointer[i] = NULL;
    } 

    return hash_table_pointer;

}


// generationg hash from the string (lexeme) using jenkins hash function

uint32_t hash_function ( char* lexeme )
{
    size_t i;
    uint32_t hash = 0;

    /* 
    * applying jenkins hash function
    * https://en.wikipedia.org/wiki/Jenkins_hash_function
    */

   while (i != strlen(lexeme)) 
   {
        hash += lexeme[i++];
        hash += hash << 10;
        hash ^= hash >> 6;
   }
    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;

    return hash % HASH_TABLE_SIZE;  // returning an index in [0, HASH_TABLE_SIZE);

}


