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

    hash_table_pointer = (entry_ht**)malloc( HASH_TABLE_SIZE * sizeof(entry_ht*) );  // allocate memory to hash table array of size HASH_TABLE_SIZE

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
    size_t i = 0;
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


/* function to create a new entry given lexeme and token_name */

entry_ht* create_entry ( char* lexeme, int token_name )
{
    entry_ht* new_entry;

    new_entry = (entry_ht*)malloc(sizeof(entry_ht*)); // allocating memory for the new entry

    new_entry -> lexeme = strdup (lexeme); // assiging the string-duplicate of lexeme to lexeme of new entry

    new_entry -> token_name = token_name;
    new_entry -> next = NULL;

    return new_entry;
}


/* function to serach for a given lexeme in the hash_table. return pointer to lexeme if exists or return NULL */

entry_ht* search (entry_ht** hash_table_ptr, char* lexeme)
{
    uint32_t idx = 0;
    entry_ht* my_entry;
    

    idx = hash_function(lexeme); // getting the index of lexeme in the hash_table

    /* traversing the linked list chain */

    my_entry = hash_table_ptr[idx];


    while (my_entry != NULL && strcmp( lexeme, my_entry -> lexeme) != 0)
    {
        my_entry = my_entry -> next;
    }
    
    if (my_entry == NULL)
    {
        return NULL; // lexeme is not found
    }

    return my_entry; // returning pointer to lexeme
    
}


/* function to insert a new entry into the hash_table given lexeme, token_name */

void insert ( entry_ht** hash_table_ptr, char* lexeme, int token_name )
{
    if (search(hash_table_ptr, lexeme) != NULL)
    {
        return; // return if lexeme already exists
    }

    uint32_t idx;
    entry_ht* new_entry;
    entry_ht* head;

    idx = hash_function ( lexeme ); // get the index of the lexeme based on the hash function
    new_entry = create_entry (lexeme, token_name); // creating a new entry

    head = hash_table_ptr[idx]; // get the head entry at this index (first link in the linked list chain)

    if (head == NULL)
    {
        hash_table_ptr[idx] = new_entry; // this is the first lexeme that has given hash index
    }
    else
    {
        new_entry -> next = hash_table_ptr[idx]; 
        hash_table_ptr[idx] = new_entry; // making the new entry head
    }

}


// Traverse the hash table and print all the entries
void display_table(entry_ht** hash_table_ptr)
{
	int i;
	entry_ht* current;

    printf("\n-------------------------------------------\n");
    printf("\t < lexeme , token >\n");
    printf("---------------------------------------------\n");

	for( i=0; i < HASH_TABLE_SIZE; i++)
	{
		current = hash_table_ptr[i];

		while( current != NULL)
		{
			printf("< %s, %d >\n", current -> lexeme, current -> token_name);
			current = current -> next;
		}
	}
    printf("------------------------------------------------\n");
   
}


int main(int argc, char *argv[])
{
    entry_ht** table = create_new_hash_table();

    insert(table, "surya", 35);
    insert(table, "nitin", 12);
    insert(table, "nag", 18);

    char new_lexeme[30];

    printf("enter your lexeme: \n");

    scanf("%[^\n]%*c", new_lexeme);
    
   

    if (search(table, new_lexeme)) {
        printf("lexeme exists");
    } else {
        printf("lexeme doesn't exists");
    }

}

