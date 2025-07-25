#ifndef CE0B1D5104534E44A2E29836848BB48C
#define CE0B1D5104534E44A2E29836848BB48C
#ifdef CE0B1D5104534E44A2E29836848BB48C

#include "zero_cost_serialization/strict_alias.h"
#include "zero_cost_serialization/interchange_float.h"
#include "zero_cost_serialization/serializable.h"
#include "zero_cost_serialization/detail/warning.h"

#include <algorithm>
#include <array>
#include <bit>
#include <compare>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <limits>
#include <numeric>
#include <tuple>
#include <type_traits>
#include <utility>
#include <span>

namespace zero_cost_serialization {
	namespace detail {
		template <typename T>
		struct underlying_type
		{
			using type = T;
		};

		template <typename T>
		requires std::is_enum_v<T>
		struct underlying_type<T>
		{
			using type = std::underlying_type_t<T>;
		};

		template <typename T>
		using underlying_integral = typename underlying_type<T>::type;

		struct integer_iterator
		{
			using iterator_concept = std::forward_iterator_tag;
			using value_type = std::size_t;
			using difference_type = std::ptrdiff_t;
			constexpr auto operator*() const noexcept { return i; }
			constexpr decltype(auto) operator++() noexcept { ++i; return *this; }
			[[maybe_unused]] constexpr auto operator++(int) noexcept { auto copy = *this; ++* this; return copy; }
			constexpr auto operator<=>(const integer_iterator&) const noexcept = default;
			value_type i;
		};
	}
	template <std::floating_point F, int M, int E, std::integral T>
	requires (std::numeric_limits<T>::digits >= (M + E) - std::is_signed_v<T>)
	struct float_constant  : std::integral_constant<T, M + E>
	{
		using float_type = F;
		static constexpr int m_bits = M;
		static constexpr int e_bits = E;
	};

	namespace detail {

		template <typename T, typename = void>
		struct is_float_bitfield_element : std::false_type {};


		template <typename T>
		struct is_float_bitfield_element <T, std::void_t<decltype(std::declval<float_constant<typename T::float_type, T::m_bits, T::e_bits, std::remove_cv_t<decltype(T::value)>>>())>>
		{
			static constexpr bool value = std::same_as<T, float_constant<typename T::float_type, T::m_bits, T::e_bits, std::remove_cv_t<decltype(T::value)>>>;
		};

		template <typename T>
		inline constexpr auto is_float_bitfield_element_v = is_float_bitfield_element<T>::value;

		template <typename T, typename = void>
		struct is_integral_bitfield_element : std::false_type {};


		template <typename T>
		struct is_integral_bitfield_element <T, std::void_t<decltype(std::declval<std::integral_constant<std::remove_cv_t<decltype(T::value)>, T::value>>())>>
		{
			static constexpr bool value = std::same_as<std::integral_constant<std::remove_cv_t<decltype(T::value)>, T::value>, T>;
		};

		template <typename T>
		inline constexpr auto is_integral_bitfield_element_v = is_integral_bitfield_element<T>::value;

		template <typename T>
		concept bitfield_param = std::disjunction_v<std::is_integral<std::remove_cvref_t<T>>, std::is_floating_point<std::remove_cvref_t<T>>, std::is_enum<std::remove_cvref_t<T>>>;
	}

	template <typename... Ts>
	requires ((detail::is_integral_bitfield_element_v<Ts> or detail::is_float_bitfield_element_v<Ts>) and ...) and
	(std::disjunction_v<std::is_integral<decltype(Ts::value)>, std::is_enum<decltype(Ts::value)>> and ...) and
	((std::numeric_limits<zero_cost_serialization::detail::corresponding_unsigned_type<detail::underlying_integral<decltype(Ts::value)>>>::digits
	>= static_cast<detail::underlying_integral<decltype(Ts::value)>>(Ts::value)) and ...)
	struct bitfield
	{
	private:
		using common_type = typename std::conditional_t<bool(sizeof...(Ts)),
			std::common_type<std::make_unsigned_t<decltype(+std::declval<detail::underlying_integral<std::remove_cv_t<decltype(Ts::value)>>>())>...>,
			std::enable_if<true, std::size_t>>::type;

		using tuple = std::tuple<Ts...>;

		using native_types = std::tuple<unsigned char, unsigned short int, unsigned int, unsigned long int, unsigned long long int>;
		using native_or_void = decltype(std::tuple_cat(std::declval<native_types>(), std::declval<std::tuple<void>>()));

		template <std::size_t N>
		static consteval auto size() noexcept
		{
			return static_cast<common_type>(std::tuple_element_t<N, tuple>::value);
		}

		template <std::size_t... S>
		static consteval auto offset(std::index_sequence<S...>) noexcept
		{
			return static_cast<common_type>((size<S>() + ...));
		}

		template <std::size_t N>
		using int_type = std::remove_cv_t<decltype(std::tuple_element_t<N, tuple>::value)>;

		template <typename T, typename = void>
		struct runtime
		{
			using type = std::remove_cv_t<decltype(T::value)>;
		};

		template <typename T>
		struct runtime<T, std::void_t<typename T::float_type>>
		{
			using type = typename T::float_type;
		};

		template <typename T, typename = void>
		struct mantissa
		{
			static constexpr int m_bits = 0;
		};

		template <typename T>
		struct mantissa<T, std::void_t<typename T::float_type>>
		{
			static constexpr int m_bits = T::m_bits;
		};

		template <typename T, typename = void>
		struct exponent
		{
			static constexpr int m_bits = 0;
		};

		template <typename T>
		struct exponent<T, std::void_t<typename T::float_type>>
		{
			static constexpr int e_bits = T::e_bits;
		};

		template <std::size_t, typename = void>
		struct runtime_or_int
		{
			using type = int;
		};

		template <std::size_t N>
		struct runtime_or_int <N, std::enable_if_t<N < sizeof...(Ts)>>
		{
			using type = typename runtime<std::tuple_element_t<N, tuple>>::type;
		};

		template <std::size_t N>
		using runtime_type = typename runtime_or_int<N>::type;

		template <std::size_t N>
		static constexpr auto m_bits = mantissa<std::tuple_element_t<N, tuple>>::m_bits;

		template <std::size_t N>
		static constexpr auto e_bits = exponent<std::tuple_element_t<N, tuple>>::e_bits;

		template <std::size_t N>
		static consteval auto offset() noexcept
		{
			if constexpr (not N)
				return common_type{};
			else
				return offset(std::make_index_sequence<N>());
		}

		template <std::size_t N>
		static consteval auto has_fast_path() noexcept
		{
			return not std::same_as<native_type<N>, void>;
		}

		static consteval auto has_conversion() noexcept
		{
			if constexpr (sizeof...(Ts) == 2)
				return std::is_same_v<runtime_type<1>, std::byte>;
			else
				return sizeof...(Ts) == 1;
		}

		static constexpr auto bits = std::size_t{ offset<sizeof...(Ts)>() };
		static constexpr auto bytes = std::size_t{ bits / std::numeric_limits<unsigned char>::digits + ((bits % std::numeric_limits<unsigned char>::digits) not_eq 0) };

		template <typename T, std::size_t N>
		static consteval auto find_index(std::size_t& i, bool& unique) noexcept
		{
			if constexpr (std::is_same_v<T, runtime_type<N>>) {
				unique = i == sizeof...(Ts);
				i = N;
			}
		}

		template <typename T, std::size_t... Is>
		static consteval auto find_index(const std::index_sequence<Is...>&) noexcept
		{
			auto unique = true;
			auto i = sizeof...(Ts);
			(find_index<T, Is>(i, unique), ...);
			if (not unique)
				i = sizeof...(Ts);
			return i;
		}

		template <typename T>
		static consteval auto index() noexcept
		{
			return find_index<T>(std::index_sequence_for<Ts...>());
		}

		template <typename T>
		static constexpr auto type_index = index<T>();

		//NOTE: enumerations in C++ are open. They can support either the range of the underlying type
		// (if fixed type) or the smallest n denoting a range [-2^(n-1), 2^(n-1)) or [0, 2^n) that can
		// hold the values of the named enumerators (for enumerations without fixed type).
		//DANGER: Convering a value to an enumeration without a fixed type e.g. enum foo { ... }
		// or enum class bar { ... } that is not in the range of the enumeration (above) is undefined
		// behavior. This property, combined with the fact that the underlying type of enumerations
		// without a fixed type is often int, means reading an enumeration without a fixed type has
		// undefined behavior for some possible values in almost all cases. Prefer fixed type
		// enumerations which do not have this pitfall.
		template <std::size_t N>
		requires (N < sizeof...(Ts) and (not size<N>() or has_fast_path<N>()))
		constexpr auto get_integral() const noexcept
		{
			if constexpr (size<N>()) {
				using underlying_type = detail::underlying_integral<int_type<N>>;
				using unsigned_type = native_type<N>;
				constexpr auto word_offset = native_offset<N>() * std::numeric_limits<unsigned char>::digits;
				constexpr auto r_shift = offset<N>() - word_offset;
				using alias_type = std::conditional_t<std::signed_integral<underlying_type>, std::make_signed_t<unsigned_type>, unsigned_type>;

				auto v = extract<alias_type, native_offset<N>()>(s);

				if constexpr (std::signed_integral<underlying_type>) {
					v <<= std::numeric_limits<unsigned_type>::digits - r_shift - size<N>();
					v >>= std::numeric_limits<unsigned_type>::digits - size<N>();
				} else {
					v >>= r_shift;
					v &= std::numeric_limits<unsigned_type>::max() >> (std::numeric_limits<unsigned_type>::digits - size<N>());
				}
				return static_cast<int_type<N>>(v);
			} else
				return int_type<N>{};
		}

		template <std::size_t N>
		requires (N < sizeof...(Ts) and (bool(size<N>()) and not has_fast_path<N>()))
		constexpr auto get_integral() const noexcept
		{
			constexpr auto s_bit = offset<N>();
			constexpr auto s_shift = bit(s_bit);
			constexpr auto n_bits = size<N>();
			constexpr auto n_first = [=]() consteval { auto a = (std::numeric_limits<unsigned char>::digits - bit(s_shift)) % std::numeric_limits<unsigned char>::digits; return std::min(a, n_bits); }();
			constexpr auto n_bytes = [=]() consteval { return (n_bits - n_first) / std::numeric_limits<unsigned char>::digits; }();
			constexpr auto l_shift = [=]() consteval { return n_first + n_bytes * std::numeric_limits<unsigned char>::digits; }();
			constexpr auto n_last = [=]() consteval { return n_bits - l_shift; }();

			using underlying_type = detail::underlying_integral<int_type<N>>;
			using unsigned_type = std::make_unsigned_t<decltype(+underlying_type())>;
			using alias_type = std::conditional_t<std::signed_integral<underlying_type>, std::make_signed_t<unsigned_type>, unsigned_type>;

			constexpr auto hi_bits = std::numeric_limits<unsigned_type>::digits - n_bits;
			constexpr auto s_adjust = std::signed_integral<alias_type> ? hi_bits : 0;

			unsigned_type v;
			auto s_first = std::span(s). template subspan<s_bit / std::numeric_limits<unsigned char>::digits>();
			auto i = std::size_t{};

			if constexpr (not n_first)
				v = {};
			else if constexpr (s_adjust < s_shift)
				v = std::to_integer<unsigned_type>(s_first[i++]) >> (s_shift - s_adjust);
			else
				v = std::to_integer<unsigned_type>(s_first[i++]) << (s_adjust - s_shift);

			for (auto d_shift = n_first; d_shift < l_shift; d_shift += std::numeric_limits<unsigned char>::digits)
				v |= std::to_integer<unsigned_type>(s_first[i++]) << (d_shift + s_adjust);

			if constexpr (n_last)
				v |= std::to_integer<unsigned_type>(s_first[i++]) << (l_shift + s_adjust);

			if constexpr (std::signed_integral<alias_type>)
				return static_cast<int_type<N>>(static_cast<alias_type>(v) >> s_adjust);
			else
				return static_cast<int_type<N>>(v & (std::numeric_limits<unsigned_type>::max() >> hi_bits));
		}

		//NOTE: If the type or underlying type (if enum) is unsigned, the value does not change
		// if the source value can be represented in the target bitfield. Otherwise the result
		// is equal to the source value modulo 2^n where n is the number of bits in the bitfield.
		// This is consistent with the behavior of builtin types.
		//-------------------------------------------------------------------------------------
		//NOTE: If the type or underlying type (if enum) is signed, the value does not change
		// if the source value can be represented in the target bitfield. Otherwise the result
		// is the unique value of the destination type equal to the source value modulo 2^n where
		// n is the number of bits in the bitfield. This is consistent with the behavior of builtin
		// types.
		//-------------------------------------------------------------------------------------
		//WARNING: Value conversion can be surprising. For example, 32768 interpreted as a signed
		// 16 bit integer is the unique value representable in the destination type, which is -32768.
		//-------------------------------------------------------------------------------------
		//DANGER: The underlying type of an enumeration with no fixed type is the type of rank equal
		// or greater than int that can support the underlying range. This usually means bitfields
		// for enumerations without a fixed type are signed, and may not be able to represent the
		// expected range. e.g. the values of enum { cat, bird, fish } are [0, 4) with 3 unnamed,
		// the underlying type is int (for conforming implementations), but a 2 bit bitfield can
		// hold the values [-2, 2). Note converting a negative value to this enumeration type is
		// undefined behavior.
		//--------------------------------------------------------------------------------------
		//DANGER: enumerations without a fixed type e.g. enum foo { ... } or enum class bar { ... }
		// ONLY have an underlying range large enough to support their enumerators fit in a small power
		// of 2. In addition, the enumeration range is open so enum { cat, bird, fish } can hold the
		// values 0, 1, 2, and 3. There is NO way to enforce not picking a larger bitfield width for
		// an enumeration value and invoking undefined behavior when copying arbitrary memory into it.
		// prefer typed enumerations, or be very careful when selecting the bitfield width to not allow
		// width larger than minimally necessary to store all enumerators.
		//-------------------------------------------------------------------------------------
		//DANGER: If the field is not aligned to a byte for both leading and trailing bits,
		// and the underlying storage is of an indeterminate value e.g. not initialized,
		// the behavior is undefined. initialize to {} if filling out your own structure.
		//-------------------------------------------------------------------------------------
		template <std::size_t N>
		requires (N < sizeof...(Ts) and (not size<N>() or has_fast_path<N>()))
		constexpr auto set_integral(const int_type<N>& value) noexcept
		{
			if constexpr (size<N>()) {
				using underlying_type = detail::underlying_integral<int_type<N>>;
				using unsigned_type = native_type<N>;
				constexpr auto word_offset = native_offset<N>() * std::numeric_limits<unsigned char>::digits;
				constexpr auto l_shift = offset<N>() - word_offset;
				using alias_type = std::conditional_t<std::signed_integral<underlying_type>, std::make_signed_t<unsigned_type>, unsigned_type>;

				constexpr auto mask = []() consteval {
					auto m = std::numeric_limits<unsigned_type>::max();
					m >>= std::numeric_limits<unsigned_type>::digits - size<N>();
					m <<= l_shift;
					return m;
				}();

				if constexpr (std::numeric_limits<unsigned_type>::digits not_eq size<N>()) {
					auto v = extract<alias_type, native_offset<N>()>(s);
					v &= ~mask;
					if constexpr (l_shift + size<N>() == std::numeric_limits<unsigned_type>::digits)
						v |= (static_cast<unsigned_type>(value) << l_shift);
					else
						v |= (static_cast<unsigned_type>(value) << l_shift) & mask;

					deposit<native_offset<N>()>(s, v);
				} else
					deposit<native_offset<N>()>(s, static_cast<alias_type>(value));
			}
		}

		template <std::size_t N>
		requires (N < sizeof...(Ts) and (bool(size<N>()) and not has_fast_path<N>()))
		constexpr auto set_integral(const int_type<N>& value) noexcept
		{
			constexpr auto d_bit = offset<N>();
			constexpr auto f_shift = bit(d_bit);
			constexpr auto n_bits = size<N>();
			constexpr auto n_first = [=]() consteval { auto a = (std::numeric_limits<unsigned char>::digits - bit(f_shift)) % std::numeric_limits<unsigned char>::digits; return std::min(a, n_bits); }();
			constexpr auto first_mask = [=]() consteval { return std::byte{ std::numeric_limits<unsigned char>::max() } >> bit(std::numeric_limits<unsigned char>::digits - n_first); }();
			constexpr auto n_bytes = [=]() consteval { return (n_bits - n_first) / std::numeric_limits<unsigned char>::digits; }();
			constexpr auto l_shift = [=]() consteval { return n_first + n_bytes * std::numeric_limits<unsigned char>::digits; }();
			constexpr auto n_last = [=]() consteval { return n_bits - l_shift; }();
			constexpr auto last_mask = [=]() consteval { return ~(std::byte{ std::numeric_limits<unsigned char>::max() } << bit(n_last)); }();

			using underlying_type = detail::underlying_integral<int_type<N>>;
			using unsigned_type = std::make_unsigned_t<decltype(+underlying_type())>;

			auto u = static_cast<unsigned_type>(value);
			auto d_first = std::span(s). template subspan<d_bit / std::numeric_limits<unsigned char>::digits>();
			auto i = std::size_t{};

			if constexpr (n_first) {
				d_first[i] &= ~(first_mask << f_shift);
				if constexpr (f_shift + n_first not_eq std::numeric_limits<unsigned char>::digits)
					d_first[i++] |= (static_cast<std::byte>(u) & first_mask) << f_shift;
				else
					d_first[i++] |= static_cast<std::byte>(u) << f_shift;
			}

			for (auto s_shift = n_first; s_shift < l_shift; s_shift += std::numeric_limits<unsigned char>::digits)
				d_first[i++] = static_cast<std::byte>(u >> s_shift);

			if constexpr (n_last) {
				d_first[i] &= ~last_mask;
				if constexpr (l_shift + n_last not_eq std::numeric_limits<unsigned char>::digits)
					d_first[i] |= static_cast<std::byte>(u >> l_shift) & last_mask;
				else
					d_first[i] |= static_cast<std::byte>(u >> l_shift);
			}
		}

		template <std::size_t N>
		struct field_proxy
		{
			bitfield* b;
			using value_type = runtime_type<N>;
			constexpr operator value_type() const noexcept
			{
				return b->get_value<N>();
			}
			constexpr auto operator=(value_type value) noexcept
			{
				b->set_value<N>(value);
				return *this;
			}
		};

		template <std::size_t N>
		requires (N < sizeof...(Ts))
		constexpr auto set_value(const runtime_type<N>& value) noexcept
		{
			if constexpr (std::is_floating_point_v<runtime_type<N>>)
				set_integral<N>(zero_cost_serialization::from_float<int_type<N>, m_bits<N>, e_bits<N>>(value));
			else
				set_integral<N>(value);
		}

		template <typename T>
		requires (type_index<T> not_eq sizeof...(Ts))
		constexpr auto set_value(const T& value) noexcept
		{
			set_value<type_index<T>>(value);
		}

		template <std::size_t N>
		requires (N < sizeof...(Ts))
		constexpr auto get_value() const noexcept
		{
			if constexpr (std::is_floating_point_v<runtime_type<N>>)
				return zero_cost_serialization::to_float<runtime_type<N>, m_bits<N>, e_bits<N>>(get_integral<N>());
			else
				return get_integral<N>();
		}

		template <typename T>
		requires (type_index<T> not_eq sizeof...(Ts))
		constexpr auto get_value() const noexcept
		{
			return get_value<type_index<T>>();
		}

		//NOTE: Alignment does not change the layout of the bitfield, instead larger alignment allows
		// potentially faster access using aligned load/store on some platforms. Native bitfields are
		// usually aligned to their largest member type. Prefer the same when possible.
		//-------------------------------------------------------------------------------------
		std::array<std::byte, bytes> s = {};
	public:
		bitfield() = default;

		template <detail::bitfield_param... Us>
		explicit constexpr bitfield(Us&&... us) noexcept
		{
			[&] <std::size_t... Is>(const std::index_sequence<Is...>&)
			{
				static_assert((std::same_as<std::remove_cvref_t<Us>, decltype(get_value<Is>())> and ...));
				(set_value<Is>(std::forward<Us>(us)), ...);
			}(std::index_sequence_for<Us...>());
		}

		template <typename T>
		requires (type_index<T> not_eq sizeof...(Ts))
		constexpr auto get() const noexcept
		{
			return get_value<type_index<T>>();
		}

		template <std::size_t N>
		requires (N < sizeof...(Ts))
		constexpr auto get() const noexcept
		{
			return get_value<N>();
		}

		template <typename T>
		requires (type_index<T> not_eq sizeof...(Ts))
		constexpr auto get() noexcept
		{
			return field_proxy<type_index<T>>{this};
		}

		template <std::size_t N>
		requires (N < sizeof...(Ts))
		constexpr auto get() noexcept
		{
			return field_proxy<N>{this};
		}

		template <typename T = runtime_type<0>>
		requires (has_conversion() and std::same_as<T, runtime_type<0>>)
		constexpr operator T() const noexcept
		{
			return get_value<0>();
		}

		constexpr auto operator=(runtime_type<0>&& t) & noexcept
		{
			static_assert(has_conversion());
			set_value(t);
			return t;
		}

		constexpr auto operator=(const runtime_type<0>& t) & noexcept
		{
			static_assert(has_conversion());
			set_value(t);
			return t;
		}

		template <typename T>
		requires (detail::tuple_like_binding<T> and std::tuple_size_v<T> == sizeof...(Ts))
		operator T() const
		{
			return [this]<std::size_t... Is>(const std::index_sequence<Is...>&)
			{
				ZERO_COST_SERIALIZATION_MISSING_BRACES
				return T{ this->get_value<Is>()... };
			}(std::index_sequence_for<Ts...>());
		}

		friend constexpr auto operator<=>(const bitfield& a, const bitfield& b) noexcept requires (zero_cost_serialization::detail::is_integral_bitfield_element_v<Ts> and ...)
		{
			return[]<std::size_t... Is>(const std::index_sequence<Is...>&, const auto& l, const auto& r)
			{
				auto o = std::strong_ordering::equal;
				((o = o == std::strong_ordering::equal ? l.template get_value<Is>() <=> r.template get_value<Is>() : o), ...);
				return o;
			}(std::index_sequence_for<Ts...>(), a, b);
		}

		friend constexpr auto operator==(const bitfield& a, const bitfield& b) noexcept requires (zero_cost_serialization::detail::is_integral_bitfield_element_v<Ts> and ...)
		{
			return a <=> b == std::strong_ordering::equal;
		}

		friend constexpr auto operator<=>(const bitfield& a, const bitfield& b) noexcept requires (zero_cost_serialization::detail::is_float_bitfield_element_v<Ts> or ...)
		{
			return[]<std::size_t... Is>(const std::index_sequence<Is...>&, const auto & l, const auto & r)
			{
				auto o = std::partial_ordering::equivalent;
				((o = o == std::partial_ordering::equivalent ? l.template get_value<Is>() <=> r.template get_value<Is>() : o), ...);
				return o;
			}(std::index_sequence_for<Ts...>(), a, b);
		}

		friend constexpr auto operator==(const bitfield& a, const bitfield& b) noexcept requires (zero_cost_serialization::detail::is_float_bitfield_element_v<Ts> or ...)
		{
			return a <=> b == std::partial_ordering::equivalent;
		}

		template <typename T, typename U>
		requires (std::same_as<std::remove_cvref_t<U>, bitfield> and type_index<T> not_eq sizeof...(Ts))
		friend constexpr auto get(U&& u) noexcept
		{
			return std::forward<U>(u). template get<T>();
		}

		template <std::size_t N, typename T>
		requires (std::same_as<std::remove_cvref_t<T>, bitfield> and N < sizeof...(Ts))
		friend constexpr auto get(T&& t) noexcept
		{
			return std::forward<T>(t). template get<N>();
		}

	private:
		static constexpr auto bit(const auto n) noexcept
		{
			return n % std::numeric_limits<unsigned char>::digits;
		}

		template <typename T, std::size_t N>
		static constexpr auto contained_in_idx(const std::size_t byte) noexcept
		{
			if constexpr (not std::has_unique_object_representations_v<T> or std::endian::native not_eq std::endian::little)
				return false;
			if (byte > bytes or bytes - byte < sizeof(T))
				return false;
			auto first_t = byte * std::numeric_limits<unsigned char>::digits;
			auto last_t = first_t + std::numeric_limits<T>::digits;
			return first_t <= offset<N>() and last_t >= offset<N>() + size<N>();
		}

		template <std::size_t N, typename T, bool aligned>
		static consteval auto native_offset() noexcept
		{
			constexpr auto byte_offset = std::size_t{ offset<N>() / std::numeric_limits<unsigned char>::digits };
			constexpr auto first = std::find_if(detail::integer_iterator{}, detail::integer_iterator{ byte_offset }, contained_in_idx<T, N>);
			constexpr auto last = std::partition_point(first, detail::integer_iterator{ bytes }, contained_in_idx<T, N>);
			if constexpr (not contained_in_idx<T, N>(*first)) return bytes;
			else if constexpr (not aligned)
				return *first;
			else {
				static_assert(first not_eq last);
				if constexpr (constexpr auto it = std::find_if(first, last, [](const auto i) { return not (i % alignof(T)); }); contained_in_idx<T, N>(*it))
					return *it;
				else
					return bytes;
			}
		}

		template <std::size_t N, bool aligned, typename... TTs, std::size_t... Is>
		static consteval auto first_type(const std::index_sequence<Is...>&, const std::tuple<TTs...>&) noexcept
		{
			return std::min(std::initializer_list<std::size_t>{(native_offset<N, TTs, aligned>() not_eq bytes ? Is : sizeof...(Is))...});
		}

		template <std::size_t N>
		using native_type = std::conditional_t<
			first_type<N, true>(std::make_index_sequence<std::tuple_size_v<native_types>>(), native_types()) not_eq std::tuple_size_v<native_types>,
			std::tuple_element_t<first_type<N, true>(std::make_index_sequence<std::tuple_size_v<native_types>>(), native_types()), native_or_void>,
			std::tuple_element_t<first_type<N, false>(std::make_index_sequence<std::tuple_size_v<native_types>>(), native_types()), native_or_void>>;

		template <std::size_t N>
		static consteval auto native_offset() noexcept
		{
			if constexpr (constexpr auto off = native_offset<N, native_type<N>, true>(); off not_eq bytes) 
				return off;
			else 
				return native_offset<N, native_type<N>, false>();
		}

		template <typename T, std::size_t N>
		static constexpr auto extract(auto& s) noexcept
		{
			alignas(T) std::array<std::byte, sizeof(T)> v;
			static_assert(std::has_unique_object_representations_v<decltype(v)>);
			std::ranges::copy(std::span(s). template subspan<N, sizeof(T)>(), v.begin());
			return std::bit_cast<T>(v);
		}

		template <std::size_t N, typename T>
		static constexpr auto deposit(auto& s, T t) noexcept
		{
			alignas(T) std::array<std::byte, sizeof(T)> v;
			static_assert(std::has_unique_object_representations_v<decltype(v)>);
			v = std::bit_cast<decltype(v)>(t);
			std::ranges::copy(v, std::span(s). template subspan<N, sizeof(T)>().begin());
		}

		template <typename, typename>
		friend struct is_serializable_type;
		template <typename>
		friend struct std::hash;
	};
	namespace detail {
		template <typename... Ts>
		struct is_bitfield : std::false_type {};
		template <typename... Ts>
		struct is_bitfield<bitfield<Ts...>> : std::true_type {};
		template <typename... Ts>
		struct is_bitfield<const bitfield<Ts...>> : std::true_type {};
		template <typename... Ts>
		struct is_bitfield<volatile bitfield<Ts...>> : std::true_type {};
		template <typename... Ts>
		struct is_bitfield<const volatile bitfield<Ts...>> : std::true_type {};

		template <typename T>
		concept reflectable_bitfield = requires
		{
			requires trivially_copyable_and_standard_layout<T>;
			requires std::regular<T>;
			requires is_bitfield<T>::value;
			requires bool(std::tuple_size_v<T>);
		};
	}

	template <detail::reflectable_bitfield B, typename Traits>
	struct is_serializable_type<B, Traits> {
		constexpr auto operator()(bool& result, std::size_t& offset, std::size_t& align) noexcept
		{
			if constexpr (not std::has_unique_object_representations_v<B>)
				result = false;
			else if (offset % alignof(B))
				result = false;
			else if constexpr (B::bits % std::numeric_limits<unsigned char>::digits)
				result = false;
			else {
				offset += sizeof(B);
				align = std::max(align, alignof(B));
			}
		}
	};
}
namespace std {
	template <typename... Ts>
	struct tuple_size<zero_cost_serialization::bitfield<Ts...>>
		: std::integral_constant<std::size_t, sizeof...(Ts)> {};

	template <std::size_t I, typename... Ts>
	struct tuple_element<I, zero_cost_serialization::bitfield<Ts...>>
	{
		using type = decltype(std::declval<zero_cost_serialization::bitfield<Ts...>&>(). template get<I>());
	};

	template <std::size_t I, typename... Ts>
	struct tuple_element<I, const zero_cost_serialization::bitfield<Ts...>>
	{
		using type = decltype(std::declval<const zero_cost_serialization::bitfield<Ts...>&>(). template get<I>());
	};

	template <typename... Ts>
	requires (zero_cost_serialization::detail::is_integral_bitfield_element_v<Ts> and ...)
	struct hash<zero_cost_serialization::bitfield<Ts...>> { constexpr auto operator()(const zero_cost_serialization::bitfield<Ts...>& b) const noexcept
	{
		constexpr auto bit_size = (Ts::value + ... + 0);
		constexpr auto trailing_bits = bit_size % std::numeric_limits<unsigned char>::digits;
		auto s = std::span(b.s). template subspan<0, bit_size / std::numeric_limits<unsigned char>::digits>();
		auto update_hash = [](auto h, auto v) { h ^= std::to_integer<decltype(h)>(v); h *= 0x100000001b3; return h; };
		auto hash = std::accumulate(s.begin(), s.end(), UINT64_C(0xcbf29ce484222325), update_hash);
		if constexpr (trailing_bits)
			return std::hash<decltype(hash)>{}(update_hash(hash, b.s.back() & ~(std::byte{ std::numeric_limits<unsigned char>::max() } << trailing_bits)));
		else
			return std::hash<decltype(hash)>{}(hash);
	}};

	template <typename... Ts>
	requires (zero_cost_serialization::detail::is_float_bitfield_element_v<Ts> or ...)
	struct hash<zero_cost_serialization::bitfield<Ts...>> { constexpr auto operator()(const zero_cost_serialization::bitfield<Ts...>& b) const noexcept
	{
		return [&] <std::size_t... Is>(const std::index_sequence<Is...>&) {
			auto hash_element = []<typename T>(auto h, T t) {
				auto update_hash = [](auto hv, auto v) { hv ^= std::to_integer<decltype(hv)>(v); hv *= 0x100000001b3; return hv; };
				auto v = std::hash<T>{}(t);
				if constexpr (std::numeric_limits<decltype(v)>::digits > std::numeric_limits<unsigned char>::digits)
					for (auto i = 0; i < std::numeric_limits<decltype(v)>::digits - std::numeric_limits<unsigned char>::digits; i += std::numeric_limits<unsigned char>::digits, v >>= std::numeric_limits<unsigned char>::digits)
						h = update_hash(h, std::byte(v));
				return update_hash(h, std::byte(v));
			};
			auto hv = UINT64_C(0xcbf29ce484222325);
			((hv = hash_element(hv, b. template get_value<Is>())), ...);
			return hv;
		}(std::index_sequence_for<Ts...>());
	}};
}

#endif
#endif
