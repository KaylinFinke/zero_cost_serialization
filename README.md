# Tools for serialization
This collection of tools is intended to support zero runtime cost serialization/deserialization idioms primarily for networking applications by leveraging native object layout. Such idioms, while common especially in low level or legacy code, are difficult to implement manually and frequently lead to undefined behavior. Reading and writing simple C structs or arrays of such objects to/from the network is fast but pitfalls such as strict aliasing violations, alignment requirements, padding, bitfields, or strange rules about enumeration types have lead to burdensome code generation solutions often with a significant code size and runtime overhead. While popular solutions do provide methods for serializing higher level objects complete with memory allocation, compression, and cross platform formats, these tools only attempt to enforce an existing cross platform format -- namely the object representation developers assume their platforms use anyway. These tools use template metaprogramming to enforce or extend features to write normal looking structures containing fixed width types, arrays, bitfields (using the provided template), and even variable length fields in the style of C flexible array members with no overhead compared to the hand-written native structures in C or C++.

# zero_cost_serialization::serializable
Most C++ implementations operate in environments with a de-facto standard representation for memory objects. This trait enforces that objects adhere to this common layout. Though the C++ standard allows a lot of variance in the memory representation of arithmetic types, platforms likely to be encountered outside of high performance computing or embedded environments have 8 bit characters, two's complement signed integral representation (required since C++20), and 16, 32, and 64 bit non-extended integral types. They also support IEEE754 binary floating point.

This trait determines if a type or set of types will be laid out identically in memory on any such platform. Out of the box, a type supports this if it is trivially copyable and standard layout (colloquially, a 'c struct'), semi-regular, does not contain bitfields, and is composed of double, float, or std::(u)intN_t, arrays/structs/enums of such types, does not support std::tuple_size, and does not contain padding. In addition, as of writing, it is common for platforms to operate in little endian environments.

Users may specialize is_serializable_type in namespace zero_cost_serialization to extend this functionality to their own types which specialize std::tuple_size, contain bitfields, or otherwise are laid out in a way they know will work correctly but cannot be automatically reflected. This has been done for a user class zero_cost_serialization::bitfield in the header "zero_cost_serialization/serializable.h". To do so implement the following template specialization:
    `zero_cost_serialization::is_serializable_type<T, Traits>` specialized for your type which contains a method `constexpr void operator()(bool& result, std::size_t& offset, std::size_t& align) noexcept` which sets result to false if T cannot be placed at this offset in a structure, assuming the structure is suitably aligned for T, as well as updates offset to include the size of T and updates align to be the max of align and the required alignment for T on any platform (if using the above rules, sizeof -- not alignof -- the largest arithmetic or enumeration type in T, or some more strictly specified alignment). Your structure should not contain padding bits.

Limitations:
1. This trait requires C++20 as it depends on __cpp_aggregate_paren_init >= 201902 (GCC 10, MSVC 19.28, Clang 16) to handle detecting structures with array members. Additionally, this uses library features requiring GCC 12.1, MSVC 19.33, and Clang 16. Newer versions of MSVC and GCC have been tested and found to work; as of writing there is no newer version of Clang. Where possible, care has been taken to achive code that compiles clean even at the highest warning levels.
2. bitfields e.g. std::uint32_t : 5 are not supported due to inability to reflect on such types. Bitfields can be emulated in a safer and more portable way guaranteed to not have padding by using the zero_cost_serialization::bitfield<> class. Note this can pack multiple types together into a bitfield just like the builtin feature would -- in addition, this class has no padding bits unlike the builtin feature in many scenarios.
3. This trait requires any class types be able to be bound to a structured binding of type auto& and composed into a tuple of references using std::tie without providing user defined specializations of std::tuple_size. The implementation limits on this are commonly 256 members.
4. This library uses fold expressions to apply methods over many sequences. Clang supports 256 elements in a fold expression as a default. User structures cannot contain more than that many objects less one at any level and be used with this library without passing compiler flags to increase this limit.
5. Some amount of misuse cannot be detected. Use of integral types other than minimum or fixed width types is not detectable since these are typedefs. Use of enums outside of the above types or with no explicitly specified underlying types is also not detectable. Fortunately, using these types is not common in a networking domain.

```
struct foo { std::uint_least32_t x; float y; };
struct bar { std::uint_least32_t x; double y; };

//Note: because we used types that don't have a fixed layout, it's possible on platforms that do not support this trait that this structure is laid out the same way as a structure that could be created on a common platform, but isn't the same on such platforms. Always check either if you're on a common platform, or use types with a fixed size that won't exist on other platforms e.g. std::uint32_t or some platform specific fixed width floating point type.

if constexpr (zero_cost_serialization::detail::zero_cost_serialization_traits::is_representation_compatible<unsigned char>()) {
    //foo is serializable on all platforms supporting this object representation.

	static_assert(zero_cost_serialization::is_serializable_v<foo>);

    //bar is not serializable on any platform that supports this object representation. On some platforms that support it bar is not padded but, because it is on some platforms e.g. AMD64 System V & AMD64 Windows this, layout is rejected.
    
	static_assert(not zero_cost_serialization::is_serializable_v<bar>);
}
```

Packs of types are supported, which model if a buffer could contain a sequence of several types which are each individually serializable. Packs support unbounded arrays e.g. T[], which are intended to model C style flexible array members. Use cases for this advanced feature involve data formats which might include some fixed size header followed by a variable number of objects. Simply supply multiple type arguments to the zero_cost_serialization::is_serializable_v trait.

# zero_cost_serialization::bitfield
This class implements a tightly packed sequence of variable width integral/enumeration types. It is a requirement that the type be able to represent as many bits as the field it represents so portable programs should, for example, have a field width not exceeding 16 for int or not exceeding 64 for std::uint_least64_t. Fixed width or minimum width integral types are recommended. The types are arranged in little-endian byte order when they straddle bytes, and access is optimized for byte aligned, word aligned, and little-endian platforms. Setting a field takes care to not clobber adjacent unused bits so padding bits at the end of a structure set to zero can safely be sent across the network. In addition, the layout of adjacent bitfields with mixed type widths or that would straddle the underlying type boundaries is consistent across platforms unlike with the native compiler types.

TIP: You can use the bitfield class to support an under-aligned or a fixed width integral or enumeration type on platforms that do not support them e.g. `zero_cost_serialization::bitfield<std::integral_constant<std::uint_least64_t, 64>>`. In fact, even if your platform supports none of the common type representations, zero_cost_serialization::bitfield supports their binary format. Support for floating point values in the bitfield class is provided using zero_cost_serialization::float_constant. See simple_rpc.cpp in examples for how to use this. Since IEEE754 is not supported everywhere, values may be rounded when converting between formats. For platforms that do support IEEE754, results are exact.

For native sized types on common platforms bitfield generates identical assembly to the builtin structures using clang, gcc, or msvc. For bitfields that fit into a native size type, bitfield generates identical assembly on clang, gcc, and msvc except for an extra instruction when padding bits would be set to an indeterminate value in the native representation and bitfield intentionally chooses to preserve their value. This allows setting what would be padding bits in bitfield's representation to 0 for serialization. For cases where the pattern of bitfield does not map to a native bitfield representation on the target architecture, the bitfield class attempts to conformantly generate what would be in practice created from a non-compliant type aliasing cast, and if that is not possible falls back to building up an integral value a byte at a time. On common platforms, this should only happen when a bitfield straddles more than 8 bytes which is not a case that can occur when modeling a native bitfield.

Bitfield is implicitly converible to tuple-like types e.g. std::tuple, std::array.

```
//1 bit for a bool, followed by 5 bits as an int_least16_t, followed by enough bits to make a multiple of CHAR_BIT.
zero_cost_serialization::bitfield<std::integral_constant<bool, 1>, std::integral_constant<std::int_least16_t, 5>> x{};
//type based accesors work if the types are unique. note enums are distinct types.
x.set_value<bool>(true); 
x.set_value<std::int_least16_t>(-3);
//index based accessors similar to std::tuple also work.
assert(x.get_value<1>() == -3);
enum { valid, slot };
//you can define a nice enum to give indices descriptive names if you like.
assert(x.get_value<valid>());
```

# zero_cost_serialization::strict_alias_cast
This is a simple wrapper around reinterpret cast which determines if the destination reference or pointer type can be used to alias the source type. This is not a transitive nor symmetric property and using this cast on a pointer or reference that is not actually the same type as the indirected object may erroneously succeed. Accessing the representation of an object through this cast is well defined behavior within the bounds of the strict aliasing rule. Use this where you would use reinterpret_cast to inspect the representation of another type.

```
int x = -7;
auto y = *zero_cost_serialization::strict_alias_cast<unsigned*>(&x); //okay, x may be inspected as an unsigned.
//auto z = *zero_cost_serialization::strict_alias_cast<float*>(&x); //fails to compile.
//auto f = *reinterpret_cast<float*>(&x); //undefined behavior.
```

# zero_cost_serialization::reinterpret_memory
This is a useful tool for type converting an existing buffer into a new type. The source object must be suitably aligned and sized to contain the new type or the behavior is undefined. This is intended to support creating an object in an aligned buffer read from the network. It also supports behavior analogous to a c style type pun through a union. NOTE: the lifetime of the original object ends if it is not an array of unsigned char or std::byte providing storage for the new type. Only refer to the objects using the resulting pointer from this cast unless referring to the original array of char/std::byte (but not its members).

```
struct foo { std::uint_least32_t x; float y; };
alignas(foo) std::byte buf[sizeof(foo)]{};
std::byte buf2[sizeof(foo)]{};
alignas(foo) std::byte buf3[sizeof(foo)];
//conformant examples:
auto p = zero_cost_serialization::reinterpret_memory<foo>(buf); //okay.
assert(p->x == 0); //x is the same representation the corresponding bytes of buf were.
p->x = 5; //okay. p really points to a foo.
p->~foo(); //buf no-longer provides storage for a foo.
new(buf)decltype(buf){}; //re-create & initialize buf. (note: buf is 'replaceable').

auto p2 = zero_cost_serialization::reinterpret_memory<foo>(buf3); //okay, but *p2 is indeterminate since buf3 was.
p2->x = 5; //okay. p2 points to a foo.
auto p3 = zero_cost_serialization::reinterpret_memory<foo>(p2); //okay. *p3 is a distinct object. *p2's lifetime has ended.
assert(p3->x == 5); //okay. representation stayed the same.
auto p4 = new(buf3) foo; //*p3 is indeterminate. p3->x isn't 5 even though we assigned it above in the same memory.

//non-conformant examples:
//auto u1 = new(buf2) foo; //probably undefined behavior, not necessarily aligned.
//auto u2 = zero_cost_serialization::reinterpret_memory<foo>(buf2); //probably undefined behavior, not necessarily aligned.

auto u3 = reinterpret_cast<foo*>(buf); //okay.
//u3->x = 5; //undefined behavior. strict aliasing violation -- no foo exists at u3 and foo isn't similar to decltype(buf).
//assert(u3->x == 0); //same as above.
new(buf) foo; //okay.
//u3->x = 5; //undefined behavior. u3 points to memory containing a foo but it wasn't obtained from the object.
//assert(u3->x == 0); //same as above. additionally, *u3 is of indeterminate value.
auto u4 = std::launder(u3); //okay.
//assert(u4->x == 0); undefined behavior (erroneous behavior in C++26), *u4 is indeterminate.
u4->x = 5; //okay.

```
This requires the source and destination types be trivially copyable.

# zero_cost_serialization::invoke
This method unpacks an aligned buffer and calls the supplied object with the templated arguments. The pack of types must be serializable. Data is passed by reference to the callable object though the parameters need not be reference parameters. Allowable parameters are 0 or more fixed size parameters and an optional unbounded array at the end modeling a variable length array. If the unbounded array is specified, the last parameter to the functor shall be a std::span (or for character types a std::basic_string_view). Returns the result of the functor.

```
template <typename... Ts>
auto test() noexcept
{
	auto fun = [](const char*, std::uint32_t u, std::span<float> f)
	{
		std::cout << u << std::endl;	
		for (auto i = std::size_t{}; i < f.size(); ++i)
			std::cout << f[i] << std::endl;
	};
	alignas(Ts...) std::byte buf[sizeof(std::uint32_t) + sizeof(float[2])]{};
	std::memcpy(buf, "hello world", sizeof(buf));
	return zero_cost_serialization::invoke<Ts...>(fun, std::forward_as_tuple("hello"), buf, sizeof(buf));
}
test<std::uint32_t, float[]>();
```

# zero_cost_serialization::apply
This method is nearly identical to the above but unpacks a reflectable class of serializable members (basically, a C struct with no padding except potentially at the end) and calls a supplied functor over the members of the struct. If the final member of the struct should be treated as a flexible array of `std::remove_extent_t<M>[]` for some element member type M depends on the signature of the functor. If the supplied object is callable with
a reference to the element types, it's a fixed size object. If the supplied object is callable with `std::span<std::remove_extent_t<M>>` as the final parameter and all other parameters as references to the element types of T, then a variable size object is created in the buffer which has 0 or more Ts.

```
	struct foobar { std::uint32_t a; float b[2]; };
	alignas(foobar) std::byte buf[sizeof(std::uint32_t) + sizeof(float[2])]{};

	auto fun = [](std::uint32_t u, float(&a)[2]) 
	{
		std::cout << u << std::endl;
		std::ranges::for_each(a, [](auto f) { std::cout << f << std::endl; });
	}; //std::uint32_t and exactly 2 floats.
	std::memcpy(buf, "hello world", sizeof(buf));
	zero_cost_serialization::apply<foobar>(fun, buf, sizeof(buf));

	auto fun2 = [](std::uint32_t u, std::span<float> p) {
		std::cout << u << std::endl;
		std::ranges::for_each(p, [](auto f) { std::cout << f << std::endl; });	
	}; //std::uint32_t and 0+ floats.
	std::memcpy(buf, "flex array!", sizeof(buf));
	zero_cost_serialization::apply<foobar>(fun2, buf, sizeof(buf));
```

# simple_rpc.cpp
An example of usage is provided that mimics a small multiplayer game where knights attempt to slay fearsome dragons. Data is slowly an inefficently copied between "client" and "server" buffers while knights try to attack dragons as fast as possible and the "server" coordinates the state of the dragons and the result of the knight's attacks. Network buffer performance and resource sharing is out of scope of this example and all use of data buffers use simple byte arrays with poor in-practice performance while all clients needlessly spam requests.
