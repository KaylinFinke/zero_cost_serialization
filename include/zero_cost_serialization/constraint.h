#ifndef C5113D33AF2E4FF29630535E073C98C7
#define C5113D33AF2E4FF29630535E073C98C7
#ifdef C5113D33AF2E4FF29630535E073C98C7
#include <stdexcept>
#include <type_traits>
#include <functional>
#include <utility>

namespace zero_cost_serialization
{
	template <typename T>
	constexpr decltype(auto) forward_if_noexcept(auto&& t) noexcept
	{
		if constexpr (not std::is_lvalue_reference_v<decltype(t)> and std::is_nothrow_constructible_v<T, decltype(t)>)
			return std::forward<decltype(t)>(t);
		else
			return std::as_const(t);
	}

	constexpr decltype(auto) forward_with_constraint(auto&& x, auto&& p)
	{
		if (not std::invoke(std::forward<decltype(p)>(p), std::as_const(x)))
			throw std::logic_error{ "Constraint violation." };
		return std::forward<decltype(x)>(x);
	}
}
#endif
#endif
