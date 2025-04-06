#ifndef A46E5576CF4E4151A22523B417E98B27
#define A46E5576CF4E4151A22523B417E98B27
#ifdef A46E5576CF4E4151A22523B417E98B27
#include <utility>
#include <concepts>
#include <istream>
#include <ostream>
#include <format>
#include <cstddef>

namespace zero_cost_serialization
{
	template <std::regular T, T null_value = T{}>
	struct nullable_resource
	{
		constexpr nullable_resource() noexcept = default;

		explicit constexpr nullable_resource(std::same_as<T> auto&& t) noexcept
			: value{std::forward<decltype(t)>(t)}
		{}
		constexpr nullable_resource(std::same_as<std::nullptr_t> auto&&) noexcept
			: value{null_value}
		{}

		explicit constexpr operator T() const& noexcept
		{
			return value;
		}
		explicit constexpr operator T() const&& noexcept
		{
			return value;
		}
		explicit constexpr operator T() && noexcept
		{
			return std::move(value);
		}
		explicit constexpr operator T() & noexcept
		{
			return value;
		}

		explicit constexpr operator bool() const noexcept
		{
			return null_value != value;
		}

		constexpr decltype(auto) operator=(std::same_as<std::nullptr_t> auto&&) noexcept
		{
			value = null_value;
			return *this;
		}

		friend constexpr auto operator<=>(const nullable_resource&, const nullable_resource&) noexcept = default;
		friend constexpr auto operator<=>(const std::nullptr_t&, const nullable_resource& rs) noexcept
		{
			return null_value <=> rs.value;
		}
		friend constexpr auto operator<=>(const nullable_resource& rs, const std::nullptr_t&) noexcept
		{
			return rs.value <=> null_value;
		}
		friend constexpr auto operator<=>(const auto& t, const nullable_resource& rs) noexcept
		{
			return t <=> rs.value;
		}
		friend constexpr auto operator<=>(const nullable_resource& rs, const auto& t) noexcept
		{
			return rs.value <=> t;
		}

		friend constexpr auto operator==(const nullable_resource&, const nullable_resource&) noexcept -> bool = default;
		friend constexpr auto operator==(const std::nullptr_t&, const nullable_resource& rs) noexcept -> bool
		{
			return null_value == rs.value;
		}
		friend constexpr auto operator==(const nullable_resource& rs, const std::nullptr_t&) noexcept -> bool
		{
			return rs.value == null_value;
		}
		friend constexpr auto operator==(const auto& t, const nullable_resource& rs) noexcept -> bool
		{
			return t == rs.value;
		}
		friend constexpr auto operator==(const nullable_resource& rs, const auto& t) noexcept -> bool
		{
			return rs.value == t;
		}

		friend constexpr auto swap(const nullable_resource& a, const nullable_resource& b) noexcept
		{
			using std::swap;
			swap(a.value, b.value);
		}

		template <typename CharT, typename Traits>
		friend decltype(auto) operator>>(std::basic_istream<CharT, Traits>& os, const nullable_resource& rs)
		{
			return os << rs.value;
		}

		template <typename CharT, typename Traits>
		friend decltype(auto) operator>>(std::basic_istream<CharT, Traits>& is, nullable_resource& rs)
		{
			return is >> rs.value;
		}

	private:
		T value = { null_value };
	};

	template <typename T>
	nullable_resource(T) -> nullable_resource<T, T{}>;
}

template <std::regular T, T null_value>
struct std::hash<zero_cost_serialization::nullable_resource<T, null_value>>
{
	using is_transparent = void;
	constexpr auto operator()(const zero_cost_serialization::nullable_resource<T, null_value>& rs) const noexcept(noexcept(std::hash<T>{}(T(rs))))
	{
		return std::hash<T>{}(T(rs));
	}
	constexpr auto operator()(const auto& t) const noexcept(noexcept(std::hash<T>{}(t)))
	{
		return std::hash<T>{}(t);
	}
};

template <std::regular T, T null_value, typename CharT>
struct std::formatter<zero_cost_serialization::nullable_resource<T, null_value>, CharT> : std::formatter<T, CharT>
{
	template <typename Iter>
	auto format(const zero_cost_serialization::nullable_resource<T, null_value>& rs, std::basic_format_context<Iter, CharT>& ctx) const
	{
		return std::formatter<T, CharT>::format(T(rs), ctx);
	}
};

#endif
#endif
