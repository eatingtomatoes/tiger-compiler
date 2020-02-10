#include <iostream>

#include "gc.hpp"

using namespace std;
    
int main()
{
    ObjectInfo* pair = (ObjectInfo*)(new char[100]);
    pair->magic = OBJECT_INFO_MAGIC;
    pair->size = 2;
    pair->fields[0] = nullptr;
    pair->fields[1] = nullptr;

    ObjectInfo* human = (ObjectInfo*)(new char[100]);
    human->magic = OBJECT_INFO_MAGIC;
    human->size =1;
    human->fields[0] = pair;

    ObjectPool pool(1024);
    BlockInfo* block = pool.allocObject(human);

    cout << "0x" << std::hex << block->magic << std::dec << endl;
    cout << "0x" << std::hex << block->info->magic << std::dec << endl;    
    
    return 0;
}

