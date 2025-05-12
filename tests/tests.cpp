#include "zero_cost_serialization/apply.h"
#include "zero_cost_serialization/bitfield.h"
#include "zero_cost_serialization/map.h"
#include "zero_cost_serialization/list.h"
#include "zero_cost_serialization/nullable_resource.h"
#include "zero_cost_serialization/scope_guard.h"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <set>
#include <type_traits>
#include <unordered_set>
#include <utility>

template <typename type>
concept boolean_testable = requires (type && t) {
	requires std::convertible_to<type, bool>;
{ not std::forward<type>(t) } -> std::convertible_to<bool>;
};
template <typename type>
concept nullable_pointer_impl = requires(const type & cr, type & r, type && rv, const std::nullptr_t & np) {
	requires std::default_initializable<type>;
	requires std::copy_constructible<type>;
	requires std::move_constructible<type>;
	requires std::equality_comparable<type>;
	requires std::destructible<type>;
	requires std::swappable<type>;
{ r = std::move(rv) } -> std::same_as<type&>;
{ r = cr } -> std::same_as<type&>;
{ type(nullptr) } -> std::same_as<type>;
{ r = nullptr } -> std::same_as<type&>;
{ cr == np } -> boolean_testable;
{ np == cr } -> boolean_testable;
{ cr != np } -> boolean_testable;
{ np != cr } -> boolean_testable;
};
template <typename type>
concept nullable_pointer = nullable_pointer_impl<::std::remove_cv_t<type>>;

static_assert(nullable_pointer<void*>);
static_assert(nullable_pointer<zero_cost_serialization::nullable_resource<int, 4>>);
static_assert(nullable_pointer<zero_cost_serialization::nullable_resource<int>>);

using i64 = std::int_least64_t;
template <i64 N>
using I64 = std::integral_constant<i64, N>;

using i32 = std::int_least32_t;
template <i32 N>
using I32 = std::integral_constant<i32, N>;

using i16 = std::int_least16_t;
template <i16 N>
using I16 = std::integral_constant<i16, N>;

using i8 = std::int_least8_t;
template <i8 N>
using I8 = std::integral_constant<i8, N>;

using u64 = std::uint_least64_t;
template <u64 N>
using U64 = std::integral_constant<u64, N>;

using u32 = std::uint_least32_t;
template <u32 N>
using U32 = std::integral_constant<u32, N>;

using u16 = std::uint_least16_t;
template <u16 N>
using U16 = std::integral_constant<u16, N>;

using u8 = std::uint_least8_t;
template <u8 N>
using U8 = std::integral_constant<u8, N>;

template<typename... Ts>
using B = zero_cost_serialization::bitfield<Ts...>;

static constexpr auto is_zero_cost_serialization = zero_cost_serialization::detail::zero_cost_serialization_traits::is_representation_compatible<unsigned char>();

//Some of the serializable tests check things on common platforms.
//These tests require an 8 bit byte due to hardcoded constants.
static_assert(zero_cost_serialization::serializable<u8> and std::numeric_limits<u8>::digits == 8);

static constexpr auto field0 = zero_cost_serialization::bitfield<>{};
static_assert(sizeof(field0) == 1);

static constexpr auto field1 = zero_cost_serialization::bitfield<std::integral_constant<signed, 7>>(-5);
static_assert(field1.get<0>() == -5); //signed fast path rank < int at 0.
[[maybe_unused]] static int unused_implicit_conversion = field1;
static_assert(sizeof(field1) == 1);
static constexpr auto field2 = zero_cost_serialization::bitfield<std::integral_constant<bool, 1>, std::integral_constant<signed, 7>>(false, -5);
static_assert(field2.get<1>() == -5); //signed fast path rank < int not at 0.
static_assert(field2.get<0>() == false); //boolean type.
static_assert(sizeof(field2) == 1);
static constexpr auto field3 = zero_cost_serialization::bitfield<std::integral_constant<int, 4>, std::integral_constant<signed long long, 62>>(0, static_cast<signed long long>(0xEAAAFFEEDDCCBBAALL));
static_assert(field3.get<1>() == static_cast<signed long long int>(0xEAAAFFEEDDCCBBAA) << (std::numeric_limits<unsigned long long int>::digits - 62) >> (std::numeric_limits<unsigned long long int>::digits - 62));
static constexpr auto field4 = zero_cost_serialization::bitfield<std::integral_constant<unsigned, 16>, std::integral_constant<signed, 8>, std::integral_constant<unsigned, 32>>(0x2211U, 0x33, 0x77665544U);
static_assert(field4.get<0>() == 0x2211); //multibyte fast path
static_assert(field4.get<1>() == 0x33); //single byte fast path
static_assert(field4.get<2>() == 0x77665544); //unaligned field.
static constexpr auto field5 = zero_cost_serialization::bitfield<std::integral_constant<int, 4>, std::integral_constant<unsigned long long, 60>>(0, 0xAAAFFEEDDCCBBAAULL);
static_assert(field5.get<1>() == static_cast<unsigned long long int>(0xAAAFFEEDDCCBBAA));
static constexpr auto field6 = zero_cost_serialization::bitfield<std::integral_constant<int, 2>, std::integral_constant<unsigned long long int, 63>>(0, 0xAAAFFEEDDCCBBAA0ULL >> 2);
static_assert(field6.get<1>() == static_cast<unsigned long long int>(0xAAAFFEEDDCCBBAA0 >> 2));
static constexpr auto field7 = [] { std::remove_const_t<decltype(field1)> f7{}; f7.get<0>() = -5; return f7; }();
static_assert(field7 == field1);
static constexpr auto field8 = [] { std::remove_const_t<decltype(field2)> f8{}; f8.get<1>() = -5; return f8; }();
static_assert(field8 == field2);
static constexpr auto field9 = [temp = field3.get<1>()] { std::remove_const_t<decltype(field3)> f9{}; f9.get<1>() = temp; return f9; }();
static_assert(field9 == field3);
static constexpr auto field10 = zero_cost_serialization::bitfield<std::integral_constant<signed, 7>>(5);
static_assert(field10 > field1);
static constexpr auto field11 = zero_cost_serialization::bitfield < std::integral_constant<bool, 1>, std::integral_constant < std::byte, std::byte{ 7 } >>(true, std::byte{0x7B});
static_assert(std::to_integer<int>(field11.get<1>()) == (-5 & 0x7F));

static_assert(not zero_cost_serialization::is_serializable_v<bool>);
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<i32>);
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<const i32>);
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<volatile i32>);
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<const volatile i32>);
using B1 = B<I64<32>, U8<5>, I16<7>, U8<4>>;
static_assert(zero_cost_serialization::is_serializable_v<B1>);
struct T1 { B1 a; u16 b; };
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T1>);
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<const T1>);
struct T2 { B1 a; i16 b; u8 c; };
static_assert(not zero_cost_serialization::is_serializable_v<T2>);
struct T3 { const B1 a; u16 b; };
static_assert(not zero_cost_serialization::is_serializable_v<T3>);
struct T4 { u16 a; u8 b, c; alignas(u32) B<I64<64>> d; };
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T4>);
struct T5 { alignas(8) B<I64<64>> a; u16 b; u8 c, d; u32 e; };
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T5>);
struct T6 { alignas(4) struct { u16 a[2]; } a; u32 b; };
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T6>);
struct T7 { struct { alignas(4) u16 a[2]; } a; u32 b; };
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T7>);

static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T5, T6>);
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<T5, T6[]>);
static_assert(not zero_cost_serialization::is_serializable_v<B1, T5>);
static_assert(zero_cost_serialization::is_serializable_v<B1[], B1[]>);
static_assert(not zero_cost_serialization::is_serializable_v<B1[], T5>);
static_assert(alignof(B1) < alignof(T1));
static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::is_serializable_v<B1[], T1>);

static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::alignment_v<double> == 8);

struct Foo {};

static_assert(not zero_cost_serialization::detail::strict_alias<int, char>);
static_assert(zero_cost_serialization::detail::strict_alias<char, int>);
static_assert(not zero_cost_serialization::detail::strict_alias<signed char, int>);
static_assert(zero_cost_serialization::detail::strict_alias<unsigned char, int>);
static_assert(zero_cost_serialization::detail::strict_alias<std::byte, int>);
static_assert(zero_cost_serialization::detail::strict_alias<unsigned, int>);
static_assert(zero_cost_serialization::detail::strict_alias<int[5], const int[]>);
static_assert(zero_cost_serialization::detail::strict_alias<const int[], int[]>);
static_assert(zero_cost_serialization::detail::strict_alias<int[], const int[5]>);
static_assert(zero_cost_serialization::detail::strict_alias<const int[5], int[5]>);
static_assert(not zero_cost_serialization::detail::strict_alias<unsigned*, int*>);
static_assert(zero_cost_serialization::detail::strict_alias<const int*, int*>);
static_assert(zero_cost_serialization::detail::strict_alias<const int* volatile*, int** const>);
static_assert(zero_cost_serialization::detail::strict_alias<const int(* volatile Foo::* const)[20], int(* const Foo::* volatile)[20]>);
static_assert(zero_cost_serialization::detail::strict_alias<int (* const*)(int*), int (* volatile*)(int*)>);
static_assert(not zero_cost_serialization::detail::strict_alias<int (Foo::*)() const, int (Foo::*)()>);
static_assert(not zero_cost_serialization::detail::strict_alias<int (*)(int*), int (*)(const int*)>);
static_assert(not zero_cost_serialization::detail::strict_alias<const int (*)(int*), int (*)(int*)>);
static_assert(zero_cost_serialization::detail::strict_alias<int (*)(int* const), int (*)(int*)>);
static_assert(not zero_cost_serialization::detail::strict_alias<std::pair<int, int>, std::pair<const int, int>>);

static_assert(std::same_as<decltype(zero_cost_serialization::strict_alias_cast<const char*>(std::declval<int*>())), const char*>);
static_assert(std::same_as<decltype(zero_cost_serialization::strict_alias_cast<char&>(std::declval<int&>())), char&>);
static_assert(std::same_as<decltype(zero_cost_serialization::strict_alias_cast<char&&>(std::declval<int&&>())), char&&>);
static_assert(std::same_as<decltype(zero_cost_serialization::strict_alias_cast<const char&&>(std::declval<const int&&>())), const char&&>);
static_assert(std::same_as<decltype(zero_cost_serialization::strict_alias_cast<const char&>(std::declval<const int&>())), const char&>);

#include <iostream>

template <typename... Ts>
auto test() noexcept
{
	auto fun = [](const char*, std::uint32_t u, std::span<zero_cost_serialization::bitfield<zero_cost_serialization::float_constant<float, 24, 8, u32>>> p)
	{
		std::cout << u << std::endl;
		std::ranges::for_each(p, [](float f) { std::cout << f << std::endl; });
	};
	alignas(Ts...) std::byte buf[12]{};
	std::ranges::copy(std::as_bytes(std::span("hello world")), buf);
	return zero_cost_serialization::invoke<Ts...>(fun, std::forward_as_tuple("hello"), buf);
}

int main()
{
	zero_cost_serialization::bitfield<zero_cost_serialization::float_constant<float, 24, 8, u32>, zero_cost_serialization::float_constant<float, 24, 8, u32>, zero_cost_serialization::float_constant<float, 24, 8, u32>> fff;
	assert(get<0>(fff) == 0.f && get<1>(fff) == 0.f && get<2>(fff) == 0.f);
	auto&& [f_i, f_j, f_k] = fff;
	f_i = 1.f;
	f_j = 2.f;
	f_k = 3.f;
	assert(get<0>(fff) == 1.f && get<1>(fff) == 2.f && get<2>(fff) == 3.f);

	[[maybe_unused]] auto test_nullable = std::hash<zero_cost_serialization::nullable_resource<int, 4>>{}(zero_cost_serialization::nullable_resource<int, 4>{});
	[[maybe_unused]] auto test_hash = std::format(L"{}", zero_cost_serialization::nullable_resource<int, 4>{});

	[[maybe_unused]] auto test_guide = zero_cost_serialization::nullable_resource{ 4 };
	static_assert(std::same_as<decltype(test_guide), zero_cost_serialization::nullable_resource<int, 0>>);

	int test_scope_guard = 7;
	{
		assert(7 == test_scope_guard);
		auto _ = zero_cost_serialization::scope_exit{ [&] { test_scope_guard = 6; } };
	}
	assert(6 == test_scope_guard);
	test_scope_guard = 7;
	{
		assert(7 == test_scope_guard);
		auto dismiss = zero_cost_serialization::scope_exit{ [&] { test_scope_guard = 6; } };
		dismiss.release();
		assert(7 == test_scope_guard);
	}
	assert(7 == test_scope_guard);
#if ZERO_COST_SERIALIZATION_HAS_EXCEPTIONS
	try {
		assert(7 == test_scope_guard);
		auto _ = zero_cost_serialization::scope_exit{ [&] { test_scope_guard = 6; } };
		throw 0;
	}
	catch (...) {
		assert(6 == test_scope_guard);
	}
#endif
	test_scope_guard = 7;
	{
		assert(7 == test_scope_guard);
		auto _ = zero_cost_serialization::scope_fail{ [&] { test_scope_guard = 6; } };
	}
	assert(7 == test_scope_guard);
	test_scope_guard = 7;
	{
		assert(7 == test_scope_guard);
		auto dismiss = zero_cost_serialization::scope_exit{ [&] { test_scope_guard = 6; } };
		dismiss.release();
		assert(7 == test_scope_guard);
	}
	assert(7 == test_scope_guard);
#if ZERO_COST_SERIALIZATION_HAS_EXCEPTIONS
	try {
		assert(7 == test_scope_guard);
		auto _ = zero_cost_serialization::scope_exit{ [&] { test_scope_guard = 6; } };
		throw 0;
	}
	catch (...) {
		assert(6 == test_scope_guard);
	}
#endif
	test_scope_guard = 7;
	{
		assert(7 == test_scope_guard);
		auto _ = zero_cost_serialization::scope_success{ [&] { test_scope_guard = 6; } };
	}
	assert(6 == test_scope_guard);
	test_scope_guard = 7;
	{
		assert(7 == test_scope_guard);
		auto dismiss = zero_cost_serialization::scope_success{ [&] { test_scope_guard = 6; } };
		dismiss.release();
		assert(7 == test_scope_guard);
	}
	assert(7 == test_scope_guard);
#if ZERO_COST_SERIALIZATION_HAS_EXCEPTIONS
	try {
		assert(7 == test_scope_guard);
		auto _ = zero_cost_serialization::scope_success{ [&] { test_scope_guard = 6; } };
		throw 0;
	}
	catch (...) {
		assert(7 == test_scope_guard);
	}

	struct throws
	{
		[[maybe_unused]] [[noreturn]] throws()
		{
			throw 0;
		}
		[[noreturn]] throws(int)
		{
			throw 0;
		}
	};

	struct throws_fn
	{
		[[maybe_unused]] throws_fn() = default;
		[[noreturn]] [[maybe_unused]] throws_fn(throws_fn&&) { throw 0; }
		[[noreturn]] throws_fn(const throws_fn&) { throw 0; }
		[[maybe_unused]] void operator()(const throws&) const noexcept {}
		void operator()(const int&) const noexcept {}
	};
#endif
	{
		auto rs = zero_cost_serialization::unique_resource{ 0, [&](int) { test_scope_guard = 6; } };
		assert(7 == test_scope_guard);
	}
	assert(6 == test_scope_guard);
	test_scope_guard = 7;
#if ZERO_COST_SERIALIZATION_HAS_EXCEPTIONS
	try {
		auto f = [&](const auto&) {test_scope_guard = 6; };
		auto rs = zero_cost_serialization::unique_resource<throws, decltype(f)> {0, f};
		assert(false);
	}
	catch (...) {
		assert(6 == test_scope_guard);
	}
	test_scope_guard = 7;
	try {
		auto rs = zero_cost_serialization::unique_resource<int, throws_fn>{ 0, throws_fn{} };
		assert(false);
	}
	catch (...) {
		assert(7 == test_scope_guard);
	}
#endif
	if constexpr (::is_zero_cost_serialization) {
		struct TestFun
		{
			void operator()(std::uint_least32_t, char) {}
		};
		struct TestFunT
		{
			std::uint_least32_t a;
			char b;
		};

		struct TestFun2
		{
			void operator()() {}
		};
		struct TestFunArgs
		{
			void operator()(std::byte*) {}
		};

		struct TestFunT2
		{
		};

		static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::apply_size_v<TestFunT, TestFun> == 5);
		static_assert(not ::is_zero_cost_serialization or zero_cost_serialization::alignment_v<std::uint_least32_t, char> == 4);
		static_assert(not ::is_zero_cost_serialization or sizeof(TestFunT) == 8);
		alignas(TestFunT) std::byte testbuf[5]{};
		zero_cost_serialization::apply<TestFunT>(TestFun{}, testbuf);
		static_assert(std::is_invocable_v<TestFun2>);
		static_assert(zero_cost_serialization::detail::is_unpack_invocable_v<TestFun2, TestFunT2>);
		static_assert(zero_cost_serialization::apply_size_v<TestFunT2, TestFun2> == 0);
		zero_cost_serialization::apply<TestFunT2>(TestFun2{}, std::span(testbuf).first(0));
		zero_cost_serialization::apply<TestFunT2>(TestFunArgs{}, std::make_tuple(static_cast<std::byte*>(testbuf)), testbuf);
	}


	zero_cost_serialization::bitfield<std::integral_constant<signed, 7>> f{};
	zero_cost_serialization::bitfield < zero_cost_serialization::float_constant<float, 24, 8, std::uint_least32_t>> g{};
	std::unordered_set<decltype(g)> g_set;
	g_set.insert(g);
	assert(g_set.contains(g));
	f = signed(-5);
	std::unordered_set<B1> u;
	u.emplace(B1{});
	u.emplace([] {B1 b{}; b.get<0>() = 1; return b; }());

	float g_float = g;
	g = g_float;

	zero_cost_serialization::bitfield<std::true_type, std::integral_constant<std::byte, std::byte{7}>> bool_test = false;
	bool xx = bool_test;
	xx = bool_test;
	bool_test = xx;

	zero_cost_serialization::bitfield<
		zero_cost_serialization::float_constant<float, 24, 8, std::uint_least32_t>,
		zero_cost_serialization::float_constant<float, 24, 8, std::uint_least32_t>,
		zero_cost_serialization::float_constant<float, 24, 8, std::uint_least32_t>> f3{};

	[[maybe_unused]] decltype(f3) other_f3 = f3;
	other_f3 = f3;
	std::array<float, 3> f3_arr = f3;
	f3_arr = f3;
	std::tuple<float, float, float> f3_tup = f3;
	f3_tup = f3;

	std::set<B1> m;
	m.emplace(B1{});
	m.emplace([] {B1 b{}; b.get<0>() = 1; return b; }());
	m.emplace([] {B1 b{}; b.get<0>() = -1; return b; }());

	assert(m.size() == 3);
	assert(u.size() == 2);
	assert(m.begin()->get<0>() == -1);
	assert(std::next(m.begin(), 2)->get<0>() == 1);
	assert(u.contains([] {B1 b{}; b.get<0>() =1; return b; }()));
	assert(u.contains(B1{}));

	assert(f.get<0>() == -5);
	f.get<0>() = -3;
	assert(signed(f.get<0>()) == -3);

	test<zero_cost_serialization::bitfield<U32<32>>, zero_cost_serialization::bitfield<zero_cost_serialization::float_constant<float, 24, 8, u32>>[]>();

	struct foobar { zero_cost_serialization::bitfield<U32<32>> a; zero_cost_serialization::bitfield<zero_cost_serialization::float_constant<float, 24, 8, u32>> b[2]; };
	alignas(foobar) std::byte buf[sizeof(std::uint32_t) + sizeof(float[2])]{};

	auto fun = [](std::uint32_t uu, zero_cost_serialization::bitfield<zero_cost_serialization::float_constant<float, 24, 8, u32>>(&a)[2])
	{
		std::cout << uu << std::endl;
		std::ranges::for_each(a, [](float ff) { std::cout << ff << std::endl; });
		return 73;
	}; //std::uint32_t and exactly 2 floats.
	std::ranges::copy(std::as_bytes(std::span("hello world")), buf);
	static_assert(zero_cost_serialization::apply_size_v<foobar, decltype(fun)> == 12);
	static_assert(zero_cost_serialization::flex_element_size_v<foobar, decltype(fun)> == 0);
	assert(73 == zero_cost_serialization::apply<foobar>(fun, buf));

	auto fun2 = [](std::uint32_t uu, std::span<zero_cost_serialization::bitfield<zero_cost_serialization::float_constant<float, 24, 8, u32>>> p) {
		std::cout << uu << std::endl;
		std::ranges::for_each(p, [](float ff) { std::cout << ff << std::endl; });
	}; //std::uint32_t and 0+ floats.
	std::ranges::copy(std::as_bytes(std::span("flex array!")), buf);
	static_assert(zero_cost_serialization::apply_size_v<foobar, decltype(fun2)> == 4);
	static_assert(zero_cost_serialization::flex_element_size_v<foobar, decltype(fun2)> == 4);
	zero_cost_serialization::apply<foobar>(fun2, buf);

	std::uint32_t ifloat = {};
	float ffloat;

	zero_cost_serialization::bitfield< zero_cost_serialization::float_constant<float, 24, 8, std::uint_least32_t>> my_float{};
	static_assert(zero_cost_serialization::detail::is_float_bitfield_element_v<zero_cost_serialization::float_constant<float, 24, 8, std::uint_least32_t>>);
	assert(0.0f == float(my_float));

	while (ifloat < 0xFFFFFFFF)
	{
		ffloat = zero_cost_serialization::to_binary32(ifloat);
		[[maybe_unused]] auto i = std::bit_cast<std::uint32_t>(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or i == ifloat);
		[[maybe_unused]] auto ii = zero_cost_serialization::from_binary32(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or ii == ifloat);
		my_float = ffloat;
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or zero_cost_serialization::from_binary32(ffloat) == zero_cost_serialization::from_binary32(float(my_float)));
		++ifloat;
	}
	{
		ffloat = zero_cost_serialization::to_binary32(ifloat);
		[[maybe_unused]] auto i = std::bit_cast<std::uint32_t>(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or i == ifloat);
		[[maybe_unused]] auto ii = zero_cost_serialization::from_binary32(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or ii == ifloat);
		my_float = ffloat;
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or zero_cost_serialization::from_binary32(ffloat) == zero_cost_serialization::from_binary32(float(my_float)));
	}
	{
		struct node
		{
			zero_cost_serialization::map::color color;
			zero_cost_serialization::map::link left;
			zero_cost_serialization::map::link right;
			zero_cost_serialization::map::link parent;
			std::size_t value;
		};
		std::array<node, 1000> A{};
		auto root = zero_cost_serialization::map::link::nil;
		for (auto i = std::size_t{}; i not_eq std::size_t{ A.size() }; ++i) {
			root = zero_cost_serialization::map::insert(A, root, i, &node::left, &node::right, &node::parent, &node::color, std::ranges::less{}, &node::value);
			assert(zero_cost_serialization::map::validate(std::span(A).first(i + 1), root, &node::left, &node::right, &node::parent, &node::color, std::ranges::less{}, &node::value));
		}
		for (auto i = std::size_t{}; i not_eq A.size(); ++i) {
			root = zero_cost_serialization::map::erase(A, root, i, &node::left, &node::right, &node::parent, &node::color);
			assert(zero_cost_serialization::map::validate(std::span(A), root, &node::left, &node::right, &node::parent, &node::color, std::ranges::less{}, &node::value));
		}
	}

	{
		using node = zero_cost_serialization::bitfield <
			std::integral_constant < zero_cost_serialization::map::color, zero_cost_serialization::map::color{ 1 } > ,
			std::integral_constant < zero_cost_serialization::map::link, zero_cost_serialization::map::link{ 21 } > ,
			std::integral_constant < zero_cost_serialization::map::link, zero_cost_serialization::map::link{ 21 } > ,
			std::integral_constant < zero_cost_serialization::map::link, zero_cost_serialization::map::link{ 21 } > ,
			std::integral_constant <std::size_t, 32 >> ;
		auto left = [](node& t) -> decltype(auto) { return get<1>(t); };
		auto right = [](node& t) -> decltype(auto) { return get<2>(t); };
		auto parent = [](node& t) -> decltype(auto) { return get<3>(t); };
		auto color = [](node& t) -> decltype(auto) { return get<0>(t); };
		auto key = [](node& t) -> decltype(auto) { return get<4>(t); };
		std::array<node, 1000> A{};
		auto root = zero_cost_serialization::map::link::nil;
		for (auto i = std::size_t{}; i not_eq A.size(); ++i) {
			root = zero_cost_serialization::map::insert(A, root, i, left, right, parent, color, std::ranges::less{}, key);
			assert(zero_cost_serialization::map::validate(std::span(A), root, left, right, parent, color, std::ranges::less{}, key));
		}
		for (auto i = std::size_t{}; i not_eq A.size(); ++i) {
			root = zero_cost_serialization::map::erase(A, root, i, left, right, parent, color);
			assert(zero_cost_serialization::map::validate(std::span(A), root, left, right, parent, color, std::ranges::less{}, key));
		}
	}
	{
		using node = std::tuple<zero_cost_serialization::map::color, zero_cost_serialization::map::link, zero_cost_serialization::map::link, zero_cost_serialization::map::link, std::size_t>;
		auto left = [](node& t) -> decltype(auto) { return get<1>(t); };
		auto right = [](node& t) -> decltype(auto) { return get<2>(t); };
		auto parent = [](node& t) -> decltype(auto) { return get<3>(t); };
		auto color = [](node& t) -> decltype(auto) { return get<0>(t); };
		auto key = [](node& t) -> decltype(auto) { return get<4>(t); };
		std::array<node, 1000> A{};
		auto root = zero_cost_serialization::map::link::nil;
		for (auto i = std::size_t{}; i not_eq A.size(); ++i) {
			root = zero_cost_serialization::map::insert(A, root, i, left, right, parent, color, std::ranges::less{}, key);
			assert(zero_cost_serialization::map::validate(std::span(A), root, left, right, parent, color, std::ranges::less{}, key));
		}
		for (auto i = std::size_t{}; i not_eq A.size(); ++i) {
			root = zero_cost_serialization::map::erase(A, root, i, left, right, parent, color);
			assert(zero_cost_serialization::map::validate(std::span(A), root, left, right, parent, color, std::ranges::less{}, key));
		}
	}

	{
		using node = zero_cost_serialization::list::link;
		std::array<node, 3> A{};
		auto head = zero_cost_serialization::list::link::nil;
		head = zero_cost_serialization::list::push_front(A, head, std::size_t{}, std::identity{});
		auto sz = std::size_t{};
		for (auto it = zero_cost_serialization::list::forward_iter{std::span(A), head, std::identity{}}; !(it == zero_cost_serialization::list::link::nil); ++it, ++sz)
			assert(&*it == A.data());
		assert(sz == 1);
		assert(zero_cost_serialization::list::validate(std::span(A), head, std::identity{}));
		sz = {};
		head = zero_cost_serialization::list::push_front(A, head, std::size_t{1}, std::identity{});
		for (auto it = zero_cost_serialization::list::forward_iter{ std::span(A), head, std::identity{} }; !(it == zero_cost_serialization::list::link::nil); ++it, ++sz)
			assert(&*it == std::span(A).subspan(1 - sz).data());
		assert(static_cast<std::size_t>(head) == 2);
		assert(zero_cost_serialization::list::validate(std::span(A), head, std::identity{}));
		assert(sz == 2);
		std::array<node, 3> B;
		std::ranges::copy(A, B.begin());
		sz = {};
		auto b_head = zero_cost_serialization::list::insert_after(B, head, zero_cost_serialization::list::link::nil, std::size_t{2}, std::identity{});
		assert(b_head == zero_cost_serialization::list::link{ 3 });
		assert(zero_cost_serialization::list::validate(std::span(B), b_head, std::identity{}));
		for (auto it = zero_cost_serialization::list::forward_iter{ std::span(B), b_head, std::identity{} }; !(it == zero_cost_serialization::list::link::nil); ++it, ++sz)
			assert(&*it == std::span(B).subspan(2 - sz).data());
		assert(sz == 3);

		std::ranges::copy(A, B.begin());
		sz = {};
		b_head = zero_cost_serialization::list::insert_after(B, head, zero_cost_serialization::list::link{1}, std::size_t{2}, std::identity{});
		assert(b_head == zero_cost_serialization::list::link{ 2 });
		assert(zero_cost_serialization::list::validate(std::span(B), b_head, std::identity{}));
		for (auto it = zero_cost_serialization::list::forward_iter{ std::span(B), b_head, std::identity{} }; !(it == zero_cost_serialization::list::link::nil); ++it, ++sz)
			assert((&*it == std::span(B).subspan(0).data() and sz == 1) or (&*it == std::span(B).subspan(2).data() and sz == 2) or (&*it == std::span(B).subspan(1).data() and sz == 0));
		assert(sz == 3);

		std::ranges::copy(A, B.begin());
		sz = {};
		b_head = zero_cost_serialization::list::insert_after(B, head, zero_cost_serialization::list::link{2}, std::size_t{2}, std::identity{});
		assert(b_head == zero_cost_serialization::list::link{2});
		assert(zero_cost_serialization::list::validate(std::span(B), b_head, std::identity{}));
		for (auto it = zero_cost_serialization::list::forward_iter{ std::span(B), b_head, std::identity{} }; !(it == zero_cost_serialization::list::link::nil); ++it, ++sz)
			assert((&*it == std::span(B).subspan(0).data() and sz == 2) or (&*it == std::span(B).subspan(2).data() and sz == 1) or (&*it == std::span(B).subspan(1).data() and sz == 0));
		assert(sz == 3);
	}
}
