#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>

#include <cassert>

#include "gc.hpp"

static ObjectPool* globalObjectPool()
{
    static ObjectPool pool(1024);
    return &pool;
}

extern "C" {
BlockInfo* allocObject(ObjectInfo* info)
{
    return globalObjectPool()->allocObject(info);
}

void mark(BlockInfo* ptr)
{
    globalObjectPool()->mark(ptr);
}
}

ObjectPool::ObjectPool(uint64_t size)
    : space_ { new BlockInfo*[size] }
    , desc_ {}
{
    desc_.push_back(BlockInfo { .magic = BLOCK_INFO_MAGIC, .info = nullptr, .object = space_.get(), .size = size, .marked = false });
}

BlockInfo* ObjectPool::allocObject(const ObjectInfo* info)
{
    assert(info);
    assert(info->magic == OBJECT_INFO_MAGIC);

    if (BlockInfo* block = tryAllocObject(info)) {
        return block;
    }

    clear();

    if (BlockInfo* block = tryAllocObject(info)) {
        return block;
    } else {
        printf("Fatal error: out of memory!\n");
        exit(-1);
    }
}

BlockInfo* ObjectPool::tryAllocObject(const ObjectInfo* info)
{
    auto iter = std::find_if(desc_.begin(), desc_.end(), [=](auto&& block) {
        return block.info == nullptr && block.size >= info->size;
    });

    if (iter == desc_.end()) {
        return nullptr;
    }

    // for (uint64_t i = 0; i < info->size; ++i) {
    //     if (const ObjectInfo* finfo = info->fields[i]) {
    //         if (BlockInfo* subobject = tryAllocObject(finfo)) {
    //             iter->object[i] = subobject;
    //         } else {
    //             return nullptr;
    //         }
    //     }
    // }

    if (iter->size > info->size) {
        auto created = desc_.insert(iter, { .magic = BLOCK_INFO_MAGIC, .info = info, .object = iter->object, .size = info->size, .marked = false });
        iter->object += info->size;
        iter->size -= info->size;
        return &*created;
    } else {
        iter->info = info;
        return &*iter;;
    }
}

void ObjectPool::mark(BlockInfo* block)
{
    assert(block);
    assert(block->magic = BLOCK_INFO_MAGIC);

    const ObjectInfo* info = block->info;

    assert(info);
    assert(info->magic == OBJECT_INFO_MAGIC);

    block->marked = true;

    for (uint64_t i = 0; i < info->size; ++i) {
        if (info->fields[i]) {
            mark(block->object[i]);
        }
    }
}

void ObjectPool::clear()
{
    auto iter = desc_.begin();
    while (iter != desc_.end()) {
        if (!iter->info)
            continue;

        if (iter->marked) {
            iter->marked = false;
        } else {
            iter->info = nullptr;

            if (iter != desc_.begin()) {
                auto previous = prev(iter);
                if (!previous->info) {
                    previous->size += iter->size;
                    iter = desc_.erase(iter);
                }
            }
        }
    }
}

std::string stringify(const ObjectInfo* info)
{
    std::ostringstream os;

    if (!info) {
        os << "nullptr";
    } else {
        os << "ObjectInfo{"
           << "magic: " << info->magic << ","
           << "size: " << info->size << ","
           << "fields:{";

        for (uint64_t i = 0; i < info->size; ++i) {
            if (info->fields[i]) {
                os << stringify(info->fields[i]);
            } else {
                os << "scalar";
            }

            if (i + 1 < info->size) {
                os << ",";
            }
        }

        os << "}}";
    }

    return os.str();
}

std::string indent(const std::string& content)
{
    std::ostringstream os;
    int level = 0;

    auto tab = [&](auto& os) {
        for (int i = 0; i < level; ++i) {
            os << "  ";
        }
    };

    for (char c : content) {
        switch (c) {
        case '{':
        case '[':
        case '(':
            os << " " << c << "\n";
            ++level;
            tab(os);
            break;
        case ',':
            os << c << "\n";
            tab(os);
            break;
        case '}':
        case ']':
        case ')':
            os << "\n";
            --level;
            tab(os);
            os << c << "\n";
            break;
        default:
            os << c;
            break;
        }
    }

    return os.str();
}
