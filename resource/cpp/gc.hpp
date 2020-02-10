#ifndef GC_H
#define GC_H

#include <memory>
#include <list>

#include <cstdint>

#ifndef OBJECT_INFO_MAGIC
#define OBJECT_INFO_MAGIC 0x1234321
#endif

#ifndef BLOCK_INFO_MAGIC
#define BLOCK_INFO_MAGIC 0x5678765
#endif

// One unit uses 8 bytes.
struct ObjectInfo {
    uint64_t magic;
    uint64_t size;
    ObjectInfo* fields[];
};

static_assert(sizeof(ObjectInfo*) == 8);
static_assert(sizeof(ObjectInfo) == 16);

struct BlockInfo {
    uint64_t magic;        
    const ObjectInfo* info;
    BlockInfo** object;
    uint64_t size;
    bool marked;
};

class ObjectPool {
public:
    ObjectPool(uint64_t size);

    BlockInfo* allocObject(const ObjectInfo* info);

    void mark(BlockInfo* ptr);

    void clear();

private:
    BlockInfo* tryAllocObject(const ObjectInfo* info);
    
private:
    std::unique_ptr<BlockInfo*[]> space_;
    std::list<BlockInfo> desc_;
};

std::string indent(const std::string& content);

std::string stringify(const ObjectInfo* info);

#endif /* GC_H */
