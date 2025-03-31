#ifndef A5FFFCB7403140D5986A80F173EDE595
#define A5FFFCB7403140D5986A80F173EDE595
#ifdef A5FFFCB7403140D5986A80F173EDE595
#include "zero_cost_serialization/detail/link.h"
#include "zero_cost_serialization/detail/platform.h"

namespace zero_cost_serialization::list {
	using detail::link;
	using detail::index_for;
	namespace detail {
		template <std::ranges::random_access_range R>
		requires std::ranges::sized_range<R>
		constexpr auto validate_index(R&& rng, list::link curr)
		{
			return list::link::nil not_eq curr and static_cast<std::size_t>(curr) <= std::forward<R>(rng).size();
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Prev>
		constexpr auto validate_link(R&& rng, list::link x, list::link x_prev, Prev prev)
		{
			return zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), x, prev) == x_prev;
		}
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	requires std::ranges::sized_range<R>
	constexpr auto validate(R&& rng, list::link x, Next next, Prev prev)
	{
		for (auto x_prev = list::link::nil; list::link::nil not_eq x; x_prev = x, x = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), x, next))
			if (not detail::validate_index(std::forward<R>(rng), x))
				return false;
			else if (not detail::validate_link(std::forward<R>(rng), x, x_prev, prev))
				return false;
		return true;
	}

	namespace detail {
		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
		auto push_front(R&& rng, list::link head, list::link x, Next next, Prev prev)
		{
			zero_cost_serialization::detail::ref(std::forward<R>(rng), x, prev) = list::link::nil;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), x, next) = head;
			if (list::link::nil not_eq head)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), head, prev) = x;
			return x;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
		auto insert_after(R&& rng, list::link head, list::link pos, list::link x, Next next, Prev prev)
		{
			if (list::link::nil not_eq pos) {
				auto y = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), pos, next);
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, prev) = pos;
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, next) = y;
				zero_cost_serialization::detail::ref(std::forward<R>(rng), pos, next) = x;
				if (list::link::nil not_eq y)
					zero_cost_serialization::detail::ref(std::forward<R>(rng), y, prev) = x;
				return head;
			} else
				return detail::push_front(std::forward<R>(rng), head, x, next, prev);
		}
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	auto push_front(R&& rng, list::link head, std::ranges::range_size_t<R> x, Next next, Prev prev)
	{
		return detail::push_front(std::forward<R>(rng), head, zero_cost_serialization::detail::make_link(x), next, prev);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	auto insert_after(R&& rng, list::link head, list::link pos, std::ranges::range_size_t<R> x, Next next, Prev prev)
	{
		return detail::insert_after(std::forward<R>(rng), head, pos, zero_cost_serialization::detail::make_link(x), next, prev);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	auto pop_front(R&& rng, list::link head, Next next, Prev prev)
	{
		auto x = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), head, next);
		if (list::link::nil not_eq x)
			zero_cost_serialization::detail::ref(std::forward<R>(rng), x, prev) = list::link::nil;
		return x;
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	auto erase_after(R&& rng, list::link head, list::link pos, Next next, Prev prev)
	{
		if (list::link::nil not_eq pos) {
			auto x = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), pos, next, next);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), pos, next) = x;
			if (list::link::nil not_eq x)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, prev) = pos;
			return head;
		} else
			return list::pop_front(std::forward<R>(rng), head, next, prev);
	}

	namespace detail {
		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
		auto erase(R&& rng, list::link head, list::link pos, Next next, Prev prev)
		{
			if (head not_eq pos) {
				auto x = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), pos, next);
				auto y = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), pos, prev);
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, next) = x;
				if (list::link::nil not_eq x)
					zero_cost_serialization::detail::ref(std::forward<R>(rng), x, prev) = y;
				return head;
			} else
				return list::pop_front(std::forward<R>(rng), head, next, prev);
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
		constexpr auto relink(R&& rng, list::link head, list::link z, list::link y, Next next, Prev prev)
		{
			auto z_prev = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z, prev);
			auto z_next = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z, next);

			auto y_prev = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), y, prev);
			auto y_next = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), y, next);

			if (list::link::nil not_eq y_next)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_next, prev) = z;
			if (list::link::nil not_eq y_prev)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_prev, next) = z;

			if (list::link::nil not_eq z_next)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_next, prev) = y;
			if (list::link::nil not_eq z_prev)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_prev, next) = y;


			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, prev) = y_prev not_eq z ? y_prev : y;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, next) = y_next not_eq z ? y_next : y;

			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, prev) = z_prev not_eq y ? z_prev : z;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, next) = z_next not_eq y ? z_next : z;

			if (z == head)
				return y;
			else if (y == head)
				return z;
			else
				return head;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
		constexpr auto transplant(R&& rng, list::link head, list::link z, list::link y, Next next, Prev prev)
		{
			if (list::link::nil not_eq zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z, prev))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, prev, next) = y;
			if (list::link::nil not_eq zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z, next))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, next, prev) = y;
			if (head == z)
				head = y;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, prev) = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z, prev);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, next) = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z, next);
			return head;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
		auto push_front(R&& rng, list::link head, list::link x, Next next)
		{
			zero_cost_serialization::detail::ref(std::forward<R>(rng), x, next) = head;
			return x;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
		auto insert_after(R&& rng, list::link head, list::link pos, list::link x, Next next)
		{
			if (list::link::nil not_eq pos) {
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, next) = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), pos, next);
				zero_cost_serialization::detail::ref(std::forward<R>(rng), pos, next) = x;
				return head;
			} else
				return detail::push_front(std::forward<R>(rng), head, x, next);
		}
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	auto erase(R&& rng, list::link head, std::ranges::range_size_t<R> pos, Next next, Prev prev)
	{
		return detail::erase(std::forward<R>(rng), head, zero_cost_serialization::detail::make_link(pos), next, prev);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	constexpr auto relink(R&& rng, list::link head, std::ranges::range_size_t<R> z, std::ranges::range_size_t<R> y, Next next, Prev prev)
	{
		return detail::relink(std::forward<R>(rng), head, zero_cost_serialization::detail::make_link(z), zero_cost_serialization::detail::make_link(y), next, prev);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next, zero_cost_serialization::detail::write_link_proj<R> Prev>
	constexpr auto transplant(R&& rng, list::link head, std::ranges::range_size_t<R> z, std::ranges::range_size_t<R> y, Next next, Prev prev)
	{
		return detail::transplant(std::forward<R>(rng), head, zero_cost_serialization::detail::make_link(z), zero_cost_serialization::detail::make_link(y), next, prev);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	auto push_front(R&& rng, list::link head, std::ranges::range_size_t<R> x, Next next)
	{
		return detail::push_front(std::forward<R>(rng), head, zero_cost_serialization::detail::make_link(x), next);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	auto insert_after(R&& rng, list::link head, list::link pos, std::ranges::range_size_t<R> x, Next next)
	{
		return detail::insert_after(std::forward<R>(rng), head, pos, zero_cost_serialization::detail::make_link(x), next);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	auto pop_front(R&& rng, list::link head, Next next)
	{
		return zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), head, next);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	auto erase_after(R&& rng, list::link head, list::link pos, Next next)
	{
		if (list::link::nil not_eq pos) {
			zero_cost_serialization::detail::ref(std::forward<R>(rng), pos, next) = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), pos, next, next);
			return head;
		} else
			return list::pop_front(std::forward<R>(rng), head, next);
	}

	namespace detail {
		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
		constexpr auto relink_after(R&& rng, list::link head, list::link z_prev, list::link y_prev, Next next)
		{
			auto z = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z_prev, next);
			auto y = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), y_prev, next);

			if (y not_eq z_prev)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_prev, next) = y;
			else
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_prev, next) = y_prev;

			if (z not_eq y_prev)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_prev, next) = z;
			else
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_prev, next) = z_prev;

			if (z_prev == head)
				return y_prev;
			else if (y_prev == head)
				return z_prev;
			else
				return head;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
		constexpr auto transplant_after(R&& rng, list::link head, list::link z_prev, list::link y, Next next)
		{
			if (z_prev == list::link::nil) {
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, next) = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), head, next);
				return y;
			} else {
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, next) = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), z_prev, next, next);
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_prev, next) = y;
				return head;
			}
		}
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	constexpr auto relink_after(R&& rng, list::link head, std::ranges::range_size_t<R> z_prev, std::ranges::range_size_t<R> y_prev, Next next)
	{
		return detail::relink_after(std::forward<R>(rng), head, zero_cost_serialization::detail::make_link(z_prev), zero_cost_serialization::detail::make_link(y_prev), next);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	constexpr auto transplant_after(R&& rng, list::link head, list::link z_prev, std::ranges::range_size_t<R> y, Next next)
	{
		return detail::transplant_after(std::forward<R>(rng), head, z_prev, zero_cost_serialization::detail::make_link(y), next);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Next>
	requires std::ranges::sized_range<R>
	constexpr auto validate(R&& rng, list::link x, Next next)
	{
		for (auto count = std::ranges::range_size_t<R>{}; list::link::nil not_eq x; ++count, x = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), x, next))
			if (count == std::forward<R>(rng).size())
				return false;
			else if (not detail::validate_index(std::forward<R>(rng), x))
				return false;
		return true;
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Next>
	requires (std::semiregular<R> and std::semiregular<Next> and std::ranges::view<R> and std::ranges::sized_range<R>)
	struct forward_iter
	{
		using value_type = std::ranges::range_value_t<R>;
		using difference_type = std::ranges::range_difference_t<R>;
		using iterator_category = std::forward_iterator_tag;

		R rng;
		list::link x;
		ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS Next next;

		constexpr decltype(auto) operator++()
		{
			x = zero_cost_serialization::detail::val<list::link>(std::forward<R>(rng), x, next);
			return *this;
		}
		constexpr auto operator++(int)
		{
			auto copy = *this;
			++*this;
			return copy;
		}
		constexpr decltype(auto) operator*() const
		{
			return rng[zero_cost_serialization::detail::index<R>(x)];
		}
		constexpr decltype(auto) operator*()
		{
			return rng[zero_cost_serialization::detail::index<R>(x)];
		}

		friend constexpr auto operator==(const forward_iter& a, const forward_iter& b) noexcept
		{
			return a.x == b.x;
		}
		friend constexpr auto operator==(list::link a, const forward_iter& b) noexcept
		{
			return a == b.x;
		}
		friend constexpr auto operator==(const forward_iter& a, list::link b) noexcept
		{
			return a.x == b;
		}

		friend constexpr auto swap(forward_iter& a, forward_iter& b) noexcept(std::conjunction_v<std::is_nothrow_swappable<R>, std::is_nothrow_swappable<Next>>)
		{
			using std::swap;
			swap(a.rng, b.rng);
			swap(a.next, b.next);
			swap(a.x, b.x);
		}
	};

	template<typename T, std::size_t Extent, typename Next>
	forward_iter(std::span<T, Extent> rng, list::link x, Next next) -> forward_iter<std::span<T, std::dynamic_extent>, Next>;

	template<typename T, typename Next>
	forward_iter(T rng, list::link x, Next next) -> forward_iter<T, Next>;
}
namespace std
{
	template <typename R, typename Next>
	struct hash<zero_cost_serialization::list::forward_iter<R, Next>>
	{
		using is_transparent = void;

		constexpr auto operator()(const zero_cost_serialization::list::forward_iter<R, Next>& it) const noexcept
		{
			return std::size_t(it.x);
		}
		constexpr auto operator()(zero_cost_serialization::list::link x) const noexcept
		{
			return std::size_t(x);
		}
	};
}
#endif
#endif
