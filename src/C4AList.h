/* by Sven2, 2001 */
// an associated list
// used by C4AulScriptEngine, as virtual function table and include/dependency list

#pragma once

#define C4AListChunkSize 128 // number of table entries for each chunk

// table entry
struct C4AListEntry
{
	void *Var, *Val;
	C4AListEntry *next(); // get entry after the given one
};

// bunch of table entries
struct C4AListChunk
{
	C4AListEntry Entries[C4AListChunkSize]; // table entries
	void *Stop; // stop entry; should always be nullptr
	C4AListChunk *Next; // next chunk
};

// associative list
class C4AList
{
protected:
	C4AListChunk *Table, *CurrC; // first/last table chunk
	int CCount; // number of entries in current chunk
	C4AListEntry *Curr; // next available entry to be used
	void Grow(); // append chunk

public:
	C4AList();
	~C4AList();
	void Clear(); // clear the list

	C4AListEntry *push(void *pVar = nullptr, void *pVal = nullptr); // push var/value pair to end of list
};
