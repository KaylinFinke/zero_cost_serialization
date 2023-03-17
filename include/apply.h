#if not defined(B0B29471F26C4810B8D8F3B0EBAE97AD)
#define B0B29471F26C4810B8D8F3B0EBAE97AD
#if defined(B0B29471F26C4810B8D8F3B0EBAE97AD)

#include "invoke.h"
#include "transparently_serializable.h"

#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>

namespace data_serialization {
	namespace detail {
		template <typename F, typename T, std::size_t... Is>
		[[nodiscard]] auto apply(const std::index_sequence<Is...>&, auto&& f, auto&& args, std::byte* data, std::size_t size)
		{
			return data_serialization::invoke<std::remove_reference_t<std::tuple_element_t<Is, T>>...>(std::forward<std::remove_reference_t<decltype(f)>>(f), std::forward<std::remove_reference_t<decltype(args)>>(args), data, size);
		}
	}

	template <common_platform::detail::reflectable_class T, typename F>
	requires common_platform::is_common_platform and detail::is_unpack_invocable_v<F, T>
	[[nodiscard]] auto apply(F&& f, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs<T>;
		return detail::apply<F, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::make_tuple(), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename Args, typename F>
	requires common_platform::is_common_platform and detail::is_unpack_invocable_v<F, T, Args>
	[[nodiscard]] auto apply(F&& f, Args&& args, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs<T>;
		return detail::apply<F, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::forward<Args>(args), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename F>
	requires common_platform::is_common_platform and detail::is_unpack_invocable_flex_v<F, T>
	[[nodiscard]] auto apply(F&& f, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs_flex<T>;
		return detail::apply<F, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::make_tuple(), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename Args, typename F>
	requires common_platform::is_common_platform and detail::is_unpack_invocable_flex_v<F, T, Args>
	[[nodiscard]] auto apply(F&& f, Args&& args, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs_flex<T>;
		return detail::apply<F, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::forward<Args>(args), data, size);
	}
}
#endif
#endif
