#pragma once

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef signed char      s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;

typedef float  f32;
typedef double f64;

typedef u8  b8;
typedef u32 b32;

#define SIZE_ASSERT(type, size) \
	static_assert(sizeof(type) == size, "Expected sizeof " #type " to be " #size " bytes.")

SIZE_ASSERT(u8, 1);
SIZE_ASSERT(u16, 2);
SIZE_ASSERT(u32, 4);
SIZE_ASSERT(u64, 8);

SIZE_ASSERT(s8, 1);
SIZE_ASSERT(s16, 2);
SIZE_ASSERT(s32, 4);
SIZE_ASSERT(s64, 8);

SIZE_ASSERT(f32, 4);
SIZE_ASSERT(f64, 8);

SIZE_ASSERT(b8, 1);
SIZE_ASSERT(b32, 4);

#undef SIZE_ASSERT

#define _CONCAT(x, y) x ## y
#define CONCAT(x, y) _CONCAT(x, y)

#define cast(type) (type)
#define ArrayLength(array) (sizeof(array) / sizeof(array[0]))

#define PACKED __attribute__((packed))

#if defined(NDEBUG) || !defined(_DEBUG)
	#define NO_ARRAY_BOUNDS_CHECK
	#define DEBUG_BREAK() do {} while (0)
	#define ASSERT(x) do { (void)x; } while (0)
#else
	#define DEBUG_BREAK() __asm__ volatile("int $0x03")
	#define ASSERT(x) \
			do { \
				if (x) { \
				} else { \
					DEBUG_BREAK(); \
				} \
			} while (0)
#endif

#define defer(x) \
	auto CONCAT(_defer_, __COUNTER__) = [](auto func) -> auto { \
		class Defer { \
		public: \
			Defer(__typeof__(func) func) \
				: Func(func) { \
			} \
			\
			~Defer() { \
				this->Func(); \
			} \
		private: \
			__typeof__(func) Func; \
		}; \
		\
		return Defer(func); \
	}([&]() -> void { x; })
