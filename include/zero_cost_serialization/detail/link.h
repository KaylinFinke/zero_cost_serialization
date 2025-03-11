#ifndef BDA706FDC7174E07BFB411887964A66F
#define BDA706FDC7174E07BFB411887964A66F
#ifdef BDA706FDC7174E07BFB411887964A66F
#include <cstddef>
#include <ranges>
#include <functional>
#include <optional>
#include <concepts>
#include <utility>

namespace zero_cost_serialization::detail {
	enum class color : bool { red, black };
	enum class link : std::size_t { nil };

	constexpr auto make_link(std::unsigned_integral auto x)
	{
		return static_cast<detail::link>(static_cast<std::size_t>(x) + 1);
	}

	template <typename R>
	constexpr auto index(detail::link x)
	{
		return static_cast<std::ranges::range_size_t<R>>(static_cast<std::ranges::range_size_t<R>>(x) - 1);
	}

	template <typename Proj, typename R, typename T>
	concept read_proj = requires(R && rng, Proj proj, detail::link r, T t)
	{
		{ t = std::invoke(proj, std::forward<R>(rng)[detail::index<R>(r)]) } -> std::same_as<T&>;
		requires std::copy_constructible<Proj>;
	};

	template <typename Proj, typename R, typename T>
	concept write_proj = requires(R && rng, Proj proj, detail::link r, T t)
	{
		{ std::invoke(proj, std::forward<R>(rng)[detail::index<R>(r)]) = t } -> std::same_as<std::invoke_result_t<Proj, decltype(std::declval<R>()[std::declval<std::ranges::range_size_t<R>>()])>>;
		requires read_proj<Proj, R, T>;
	};

	template <typename Proj, typename R>
	concept write_color_proj = write_proj<Proj, R, detail::color>;

	template <typename Proj, typename R>
	concept write_link_proj = write_proj<Proj, R, detail::link>;

	template <typename Proj, typename R>
	concept read_color_proj = read_proj<Proj, R, detail::color>;

	template <typename Proj, typename R>
	concept read_link_proj = read_proj<Proj, R, detail::link>;

	template <typename R, typename... P, std::size_t... Is>
	constexpr decltype(auto) ref_helper(R&& rng, detail::link x, std::index_sequence<Is...>, P... proj)
	{
		auto r = x;
		auto last = (..., [&] { if constexpr (Is not_eq sizeof...(P) - 1) r = std::invoke(proj, std::forward<R>(rng)[detail::index<R>(r)]); else return proj; }());
		return std::invoke(last, std::forward<R>(rng)[detail::index<R>(r)]);
	}

	template <typename R, typename... P>
	constexpr decltype(auto) ref(R&& rng, detail::link x, P... proj)
	{
		return detail::ref_helper(std::forward<R>(rng), x, std::index_sequence_for<P...>(), std::forward<P>(proj)...);
	}

	template <typename T, typename R, typename... P, std::size_t... Is>
	constexpr auto val_helper(R&& rng, detail::link x, std::index_sequence<Is...>, P... proj) -> T
	{
		auto r = x;
		auto last = (..., [&] { if constexpr (Is not_eq sizeof...(P) - 1) r = std::invoke(proj, std::forward<R>(rng)[detail::index<R>(r)]); else return proj; }());
		if constexpr (std::same_as<T, detail::color>) {
			if (detail::link::nil == r)
				return detail::color::black;
			return std::invoke(last, std::forward<R>(rng)[detail::index<R>(r)]);
		}
		else {
			static_assert(std::same_as<T, detail::link>);
			return std::invoke(last, std::forward<R>(rng)[detail::index<R>(r)]);
		}
	}

	template <typename T, typename R, typename... P>
	constexpr auto val(R&& rng, detail::link x, P... proj)
	{
		return detail::val_helper<T>(std::forward<R>(rng), x, std::index_sequence_for<P...>(), proj...);
	}
}

namespace std
{
	template <>
	struct hash<zero_cost_serialization::detail::link>
	{
		constexpr auto operator()(zero_cost_serialization::detail::link x) const noexcept
		{
			return std::size_t(x);
		}
	};
}
#endif
#endif
