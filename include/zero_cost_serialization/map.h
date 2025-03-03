#ifndef EF2704183C34412987AC7B604BAB84A7
#define EF2704183C34412987AC7B604BAB84A7
#ifdef EF2704183C34412987AC7B604BAB84A7

#include <ranges>
#include <functional>
#include <optional>
#include <concepts>
#include <utility>
#include "zero_cost_serialization/detail/platform.h"

namespace zero_cost_serialization::map {
	namespace detail {

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
		concept read_proj = requires(R&& rng, Proj proj, detail::link r, T t)
		{
			{ t = std::invoke(proj, std::forward<R>(rng)[detail::index<R>(r)]) } -> std::same_as<T&>;
			requires std::copy_constructible<Proj>;
		};

		template <typename Proj, typename R, typename T>
		concept write_proj = requires(R&& rng, Proj proj, detail::link r, T t)
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
			} else {
				static_assert(std::same_as<T, detail::link>);
				return std::invoke(last, std::forward<R>(rng)[detail::index<R>(r)]);
			}
		}

		template <typename T, typename R, typename... P>
		constexpr auto val(R&& rng, detail::link x, P... proj)
		{
			return detail::val_helper<T>(std::forward<R>(rng), x, std::index_sequence_for<P...>(), proj...);
		}

		template <typename R,  typename Left, typename Right, typename Parent>
		constexpr auto left_rotate(R&& rng, detail::link root, detail::link x, Left left, Right right, Parent parent)
		{
			const auto y = detail::val<detail::link>(std::forward<R>(rng), x, right);
			detail::ref(std::forward<R>(rng), x, right) = detail::val<detail::link>(std::forward<R>(rng), y, left);
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), y, left))
				detail::ref(std::forward<R>(rng), y, left, parent) = x;
			detail::ref(std::forward<R>(rng), y, parent) = detail::val<detail::link>(std::forward<R>(rng), x, parent);
			if (detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), x, parent))
				root = y;
			else if (x == detail::val<detail::link>(std::forward<R>(rng), x, parent, left))
				detail::ref(std::forward<R>(rng), x, parent, left) = y;
			else
				detail::ref(std::forward<R>(rng), x, parent, right) = y;
			detail::ref(std::forward<R>(rng), y, left) = x;
			detail::ref(std::forward<R>(rng), x, parent) = y;
			return root;
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto left_insert_fixup(R&& rng, detail::link root, detail::link z, Left left, Right right, Parent parent, Color color)
		{
			const auto y = detail::val<detail::link>(std::forward<R>(rng), z, parent, parent, right);
			if (detail::color::red == detail::val<detail::color>(std::forward<R>(rng), y, color)) {
				detail::ref(std::forward<R>(rng), z, parent, color) = detail::color::black;
				detail::ref(std::forward<R>(rng), y, color) = detail::color::black;
				detail::ref(std::forward<R>(rng), z, parent, parent, color) = detail::color::red;
				z = detail::val<detail::link>(std::forward<R>(rng), z, parent, parent);
			} else {
				if (z == detail::val<detail::link>(std::forward<R>(rng), z, parent, right)) {
					z = detail::val<detail::link>(std::forward<R>(rng), z, parent);
					root = detail::left_rotate(std::forward<R>(rng), root, z, left, right, parent);
				}
				detail::ref(std::forward<R>(rng), z, parent, color) = detail::color::black;
				detail::ref(std::forward<R>(rng), z, parent, parent, color) = detail::color::red;
				root = detail::left_rotate(std::forward<R>(rng), root, detail::val<detail::link>(std::forward<R>(rng), z, parent, parent), right, left, parent);
			}
			return std::pair{ root, z };
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto insert_fixup(R&& rng, detail::link root, detail::link z, Left left, Right right, Parent parent, Color color)
		{
			while (detail::color::red == detail::val<detail::color>(std::forward<R>(rng), z, parent, color))
				if (detail::val<detail::link>(std::forward<R>(rng), z, parent) == detail::val<detail::link>(std::forward<R>(rng), z, parent, parent, left))
					std::tie(root, z) = detail::left_insert_fixup(std::forward<R>(rng), root, z, left, right, parent, color);
				else
					std::tie(root, z) = detail::left_insert_fixup(std::forward<R>(rng), root, z, right, left, parent, color);
			detail::ref(std::forward<R>(rng), root, color) = detail::color::black;
			return root;
		}

		template <typename R, typename Right>
		constexpr auto max(R&& rng, detail::link x, Right right)
		{
			while (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), x, right))
				x = detail::val<detail::link>(std::forward<R>(rng), x, right);
			return x;
		}

		template <typename R, typename Left, typename Right, typename Parent>
		constexpr auto successor(R&& rng, detail::link x, Left left, Right right, Parent parent)
		{
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), x, right))
				return detail::max(std::forward<R>(rng), detail::val<detail::link>(std::forward<R>(rng), x, right), left);
			auto y = detail::val<detail::link>(std::forward<R>(rng), x, parent);
			while (detail::link::nil not_eq y and x == detail::val<detail::link>(std::forward<R>(rng), y, right)) {
				x = y;
				y = detail::val<detail::link>(std::forward<R>(rng), y, parent);
			}
			return y;
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto left_delete_fixup(R&& rng, detail::link root, detail::link x, detail::link xp, Left left, Right right, Parent parent, Color color)
		{
			auto w = detail::val<detail::link>(std::forward<R>(rng), xp, right);
			if (detail::color::red == detail::val<detail::color>(std::forward<R>(rng), w, color)) {
				detail::ref(std::forward<R>(rng), w, color) = detail::color::black;
				detail::ref(std::forward<R>(rng), xp, color) = detail::color::red;
				root = detail::left_rotate(std::forward<R>(rng), root, xp, left, right, parent);
				w = detail::val<detail::link>(std::forward<R>(rng), xp, right);
			}
			if (detail::color::black == detail::val<detail::color>(std::forward<R>(rng), w, left, color) and detail::color::black == detail::val<detail::color>(std::forward<R>(rng), w, right, color)) {
				detail::ref(std::forward<R>(rng), w, color) = detail::color::red;
				x = xp;
				xp = detail::val<detail::link>(std::forward<R>(rng), x, parent);
			} else {
				if (detail::color::black == detail::val<detail::color>(std::forward<R>(rng), w, right, color)) {
					detail::ref(std::forward<R>(rng), w, left, color) = detail::color::black;
					detail::ref(std::forward<R>(rng), w, color) = detail::color::red;
					root = detail::left_rotate(std::forward<R>(rng), root, w, right, left, parent);
					w = detail::val<detail::link>(std::forward<R>(rng), xp, right);
				}
				detail::ref(std::forward<R>(rng), w, color) = detail::val<detail::color>(std::forward<R>(rng), xp, color);
				detail::ref(std::forward<R>(rng), xp, color) = detail::color::black;
				if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), w, right))
					detail::ref(std::forward<R>(rng), w, right, color) = detail::color::black;
				x = root = detail::left_rotate(std::forward<R>(rng), root, xp, left, right, parent);
			}
			return std::tuple{ root, x, xp };
		}


		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto delete_fixup(R&& rng, detail::link root, detail::link x, detail::link xp, Left left, Right right, Parent parent, Color color)
		{
			while (root not_eq x and detail::color::black == detail::val<detail::color>(std::forward<R>(rng), x, color))
				if (detail::link::nil not_eq xp and detail::val<detail::link>(std::forward<R>(rng), xp, left) == x)
					std::tie(root, x, xp) = detail::left_delete_fixup(std::forward<R>(rng), root, x, xp, left, right, parent, color);
				else
					std::tie(root, x, xp) = detail::left_delete_fixup(std::forward<R>(rng), root, x, xp, right, left, parent, color);
			if (detail::link::nil not_eq x)
				detail::ref(std::forward<R>(rng), x, color) = detail::color::black;
			return root;
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto transplant(R&& rng, detail::link root, detail::link z, detail::link y, Left left, Right right, Parent parent, Color color)
		{
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), z, parent)) {
				if (detail::val<detail::link>(std::forward<R>(rng), z, parent, left) == z)
					detail::ref(std::forward<R>(rng), z, parent, left) = y;
				else
					detail::ref(std::forward<R>(rng), z, parent, right) = y;
			}
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), z, left))
				detail::ref(std::forward<R>(rng), z, left, parent) = y;
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), z, right))
				detail::ref(std::forward<R>(rng), z, right, parent) = y;
			if (root == z)
				root = y;
			detail::ref(std::forward<R>(rng), y, color) = detail::val<detail::color>(std::forward<R>(rng), z, color);
			detail::ref(std::forward<R>(rng), y, left) = detail::val<detail::link>(std::forward<R>(rng), z, left);
			detail::ref(std::forward<R>(rng), y, right) = detail::val<detail::link>(std::forward<R>(rng), z, right);
			detail::ref(std::forward<R>(rng), y, parent) = detail::val<detail::link>(std::forward<R>(rng), z, parent);
			return root;
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color, typename Less = std::ranges::less, typename Key = std::identity>
		constexpr auto insert(R&& rng, detail::link root, detail::link z, Left left, Right right, Parent parent, Color color, Less less = {}, Key key = {})
		{
			auto y = detail::link::nil;
			auto x = root;
			while (detail::link::nil not_eq x) {
				y = x;
				if (std::invoke(less, detail::ref(std::forward<R>(rng), z, key), detail::ref(std::forward<R>(rng), x, key)))
					x = detail::val<detail::link>(std::forward<R>(rng), x, left);
				else
					x = detail::val<detail::link>(std::forward<R>(rng), x, right);
			}
			detail::ref(std::forward<R>(rng), z, color) = detail::color::red;
			detail::ref(std::forward<R>(rng), z, left) = detail::link::nil;
			detail::ref(std::forward<R>(rng), z, right) = detail::link::nil;
			detail::ref(std::forward<R>(rng), z, parent) = y;
			if (detail::link::nil == y)
				root = z;
			else if (std::invoke(less, detail::ref(std::forward<R>(rng), z, key), detail::ref(std::forward<R>(rng), y, key)))
				detail::ref(std::forward<R>(rng), y, left) = z;
			else
				detail::ref(std::forward<R>(rng), y, right) = z;
			return detail::insert_fixup(std::forward<R>(rng), root, z, left, right, parent, color);
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto erase(R&& rng, detail::link root, detail::link z, Left left, Right right, Parent parent, Color color)
		{
			auto y = z;
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), z, left) and detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), z, right))
				y = detail::successor(std::forward<R>(rng), z, left, right, parent);
			auto y_child = detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), y, left) ? detail::val<detail::link>(std::forward<R>(rng), y, left) : detail::val<detail::link>(std::forward<R>(rng), y, right);
			if (detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), y, parent))
				root = y_child;
			else if (y == detail::val<detail::link>(std::forward<R>(rng), y, parent, left))
				detail::ref(std::forward<R>(rng), y, parent, left) = y_child;
			else
				detail::ref(std::forward<R>(rng), y, parent, right) = y_child;
			if (detail::link::nil not_eq y_child)
				detail::ref(std::forward<R>(rng), y_child, parent) = detail::val<detail::link>(std::forward<R>(rng), y, parent);
			auto y_color = detail::val<detail::color>(std::forward<R>(rng), y, color);
			auto y_child_parent = detail::val<detail::link>(std::forward<R>(rng), y, parent) == z ? y : detail::val<detail::link>(std::forward<R>(rng), y, parent);
			if (y not_eq z)
				root = detail::transplant(std::forward<R>(rng), root, z, y, left, right, parent, color);
			if (detail::color::black == y_color)
				root = detail::delete_fixup(std::forward<R>(rng), root, y_child, y_child_parent, left, right, parent, color);
			return root;
		}

		template <typename R>
		constexpr auto validate_index(R&& rng, detail::link curr)
		{
			return detail::link::nil not_eq curr and static_cast<std::size_t>(curr) <= rng.size();
		}

		template <typename R, typename Left, typename Right, typename Parent>
		constexpr auto validate_link(R&& rng, detail::link curr, detail::link next, Left left, Right right, Parent parent)
		{
			return detail::val<detail::link>(std::forward<R>(rng), curr, parent) == next and (detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), curr, left) or detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), curr, right) or detail::val<detail::link>(std::forward<R>(rng), curr, left) not_eq detail::val<detail::link>(std::forward<R>(rng), curr, right));
		}


		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto validate_min(R&& rng, detail::link curr, std::ranges::range_size_t<R> black_height, Left left, Right right, Parent parent, Color color) -> decltype(std::make_optional(std::pair{ curr, black_height }))
		{
			for (; detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), curr, left); curr = detail::val<detail::link>(std::forward<R>(rng), curr, left)) {
				if (not detail::validate_index(std::forward<R>(rng), detail::val<detail::link>(std::forward<R>(rng), curr, left)))
					return std::nullopt;
				if (not detail::validate_link(std::forward<R>(rng), detail::val<detail::link>(std::forward<R>(rng), curr, left), curr, left, right, parent))
					return std::nullopt;
				if (detail::val<detail::color>(std::forward<R>(rng), curr, left, color) == detail::color::black)
					++black_height;
			}
			return std::make_optional(std::pair{ curr, black_height });
		}

		template <typename R, typename Left, typename Right, typename Parent, typename Color>
		constexpr auto validate_successor(R&& rng, detail::link curr, std::ranges::range_size_t<R> black_height, Left left, Right right, Parent parent, Color color) -> decltype(std::make_optional(std::pair{ curr, black_height }))
		{
			if (detail::link::nil not_eq detail::val<detail::link>(std::forward<R>(rng), curr, right)) {
				if (not detail::validate_index(std::forward<R>(rng), detail::val<detail::link>(std::forward<R>(rng), curr, right)))
					return std::nullopt;
				if (not detail::validate_link(std::forward<R>(rng), detail::val<detail::link>(std::forward<R>(rng), curr, right), curr, left, right, parent))
					return std::nullopt;
				if (detail::val<detail::color>(std::forward<R>(rng), curr, right, color) == detail::color::black)
					++black_height;
				if (auto min = detail::validate_min(std::forward<R>(rng), detail::val<detail::link>(std::forward<R>(rng), curr, right), black_height, left, right, parent, color))
					std::tie(curr, black_height) = min.value();
				else
					return std::nullopt;
			} else
				for (auto done = false; not done; done = detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), curr, parent) or detail::val<detail::link>(std::forward<R>(rng), curr, parent, right) not_eq curr, curr = detail::val<detail::link>(std::forward<R>(rng), curr, parent))
					if (detail::val<detail::color>(std::forward<R>(rng), curr, color) == detail::color::black)
						--black_height;
			return std::make_optional(std::pair{curr, black_height});
		}

		struct min_fn
		{
			template <std::ranges::random_access_range R, detail::write_link_proj<R> Left>
			constexpr auto operator()(R&& rng, detail::link root, Left left) const
			{
				return detail::link::nil not_eq root ? detail::max(std::forward<R>(rng), root, left) : root;
			}
		};

		struct max_fn
		{
			template <std::ranges::random_access_range R, detail::write_link_proj<R> Right>
			constexpr auto operator()(R&& rng, detail::link root, Right right) const
			{
				return detail::link::nil not_eq root ? detail::max(std::forward<R>(rng), root, right) : root;
			}
		};

		struct insert_fn
		{
			template <std::ranges::random_access_range R, detail::write_link_proj<R> Left, detail::write_link_proj<R> Right, detail::write_link_proj<R> Parent, detail::write_color_proj<R> Color, typename Less = std::ranges::less, typename Key = std::identity>
			requires (std::indirect_strict_weak_order<Less, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
			constexpr auto operator()(R&& rng, detail::link root, std::ranges::range_size_t<R> index, Left left, Right right, Parent parent, Color color, Less less = {}, Key key = {}) const
			{
				return detail::insert(std::forward<R>(rng), root, detail::make_link(index), left, right, parent, color, less, key);
			}
		};

		struct erase_fn
		{
			template <std::ranges::random_access_range R, detail::write_link_proj<R> Left, detail::write_link_proj<R> Right, detail::write_link_proj<R> Parent, detail::write_color_proj<R> Color>
			constexpr auto operator()(R&& rng, detail::link root, std::ranges::range_size_t<R> index, Left left, Right right, Parent parent, Color color) const
			{
				return detail::erase(std::forward<R>(rng), root, detail::make_link(index), left, right, parent, color);
			}
		};

		struct validate_fn
		{
			template <std::ranges::random_access_range R, detail::read_link_proj<R> Left, detail::read_link_proj<R> Right, detail::read_link_proj<R> Parent, detail::read_color_proj<R> Color, typename Less = std::ranges::less, typename Key = std::identity>
			requires (std::indirect_strict_weak_order<Less, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
			constexpr auto operator()(R&& rng, detail::link root, Left left, Right right, Parent parent, Color color, Less less = {}, Key key = {}) const
			{
				auto black_height = std::ranges::range_size_t<R>{1};
				if (detail::link::nil == root)
					return true;
				if (not detail::validate_index(std::forward<R>(rng), root))
					return false;
				if (not detail::validate_link(std::forward<R>(rng), root, detail::link::nil, left, right, parent))
					return false;
				if (detail::val<detail::color>(std::forward<R>(rng), root, color) not_eq detail::color::black)
					return false;
				if (auto min = detail::validate_min(std::forward<R>(rng), root, black_height, left, right, parent, color))
					std::tie(root, black_height) = min.value();
				else
					return false;
				auto curr_height = black_height;
				for (auto prev = root; detail::link::nil not_eq prev; prev = root) {
					if ((detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), root, left) or detail::link::nil == detail::val<detail::link>(std::forward<R>(rng), root, right)) and curr_height not_eq black_height)
						return false;
					if (detail::val<detail::color>(std::forward<R>(rng), root, color) == detail::color::red and detail::val<detail::color>(std::forward<R>(rng), root, parent, color) == detail::color::red)
						return false;

					if (auto next = detail::validate_successor(std::forward<R>(rng), root, curr_height, left, right, parent, color))
						std::tie(root, curr_height) = next.value();
					else
						return false;

					if (detail::link::nil not_eq root and std::invoke(less, detail::ref(std::forward<R>(rng), root, key), detail::ref(std::forward<R>(rng), prev, key)))
						return false;
				}
				return true;
			}
		};

		struct lower_bound_fn
		{
			template <std::ranges::random_access_range R, typename V, detail::read_link_proj<R> Left, detail::read_link_proj<R> Right, typename Less = std::ranges::less, typename Key = std::identity>
			requires (std::indirect_strict_weak_order<Less, const V*, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
			auto operator()(R&& rng, detail::link x, const V& value, Left left, Right right, Less less = {}, Key key = {}) const
			{
				auto z = detail::link::nil;
				while (detail::link::nil not_eq x)
					if (not std::invoke(less, detail::ref(std::forward<R>(rng), x, key), value)) {
						z = x;
						x = detail::val<detail::link>(std::forward<R>(rng), x, left);
					} else
						x = detail::val<detail::link>(std::forward<R>(rng), x, right);
				return z;
			}
		};

		struct upper_bound_fn
		{
			template <typename R, typename V, detail::read_link_proj<R> Left, detail::read_link_proj<R> Right, typename Less = std::ranges::less, typename Key = std::identity>
			requires (std::indirect_strict_weak_order<Less, const V*, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
			auto operator()(R&& rng, detail::link x, const V& value, Left left, Right right, Less less = {}, Key key = {}) const
			{
				auto z = detail::link::nil;
				while (detail::link::nil not_eq x)
					if (std::invoke(less, value, detail::ref(std::forward<R>(rng), x, key))) {
						z = x;
						x = detail::val<detail::link>(std::forward<R>(rng), x, left);
					} else
						x = detail::val<detail::link>(std::forward<R>(rng), x, right);
				return z;
			}
		};

		struct find_fn
		{
			template <std::ranges::random_access_range R, typename V, detail::read_link_proj<R> Left, detail::read_link_proj<R> Right, typename Less = std::ranges::less, typename Key = std::identity>
			requires (std::indirect_strict_weak_order<Less, const V*, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
			auto operator()(R&& rng, detail::link x, const V& value, Left left, Right right, Less less = {}, Key key = {}) const
			{
				x = detail::lower_bound_fn{}(std::forward<R>(rng), x, value, left, right, less, key);
				if (detail::link::nil not_eq x and not std::invoke(less, value, detail::ref(std::forward<R>(rng), x, key)))
					return x;
				return detail::link::nil;
			}
		};

		template <std::ranges::random_access_range R, detail::read_link_proj<R> Left, detail::read_link_proj<R> Right, detail::read_link_proj<R> Parent>
		requires (std::semiregular<R> and std::semiregular<Left> and std::semiregular<Right> and std::semiregular<Parent> and std::ranges::view<R> and std::ranges::sized_range<R>)
		struct forward_iter
		{
			using value_type = std::ranges::range_value_t<R>;
			using difference_type = std::ranges::range_difference_t<R>;
			using iterator_category = std::forward_iterator_tag;

			R rng;
			detail::link x;
			ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS Left left;
			ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS Right right;
			ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS Parent parent;

			constexpr decltype(auto) operator++()
			{
				x = detail::successor(rng, x, left, right, parent);
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
				return rng[detail::index<R>(x)];
			}
			constexpr decltype(auto) operator*()
			{
				return rng[detail::index<R>(x)];
			}

			friend constexpr auto operator==(const forward_iter& a, const forward_iter& b) noexcept
			{
				return a.x == b.x;
			}
			friend constexpr auto operator==(detail::link a, const forward_iter& b) noexcept
			{
				return a == b.x;
			}
			friend constexpr auto operator==(const forward_iter& a, detail::link b) noexcept
			{
				return a.x == b;
			}

			friend constexpr auto swap(forward_iter& a, forward_iter& b) noexcept(std::conjunction_v<std::is_nothrow_swappable<R>, std::is_nothrow_swappable<Left>, std::is_nothrow_swappable<Right>, std::is_nothrow_swappable<Parent>>)
			{
				using std::swap;
				swap(a.rng, b.rng);
				swap(a.left, b.left);
				swap(a.right, b.right);
				swap(a.parent, b.parent);
				swap(a.x, b.x);
			}
		};

		template<typename T, std::size_t Extent, typename Left, typename Right, typename Parent>
		forward_iter(std::span<T, Extent> rng, detail::link x, Left left, Right right, Parent parent) -> forward_iter<std::span<T, std::dynamic_extent>, Left, Right, Parent>;
	}

	using detail::color;
	using detail::link;
	[[maybe_unused]] inline constexpr auto min_element = detail::min_fn{};
	[[maybe_unused]] inline constexpr auto max_element = detail::max_fn{};
	[[maybe_unused]] inline constexpr auto find = detail::find_fn{};
	[[maybe_unused]] inline constexpr auto lower_bound = detail::lower_bound_fn{};
	[[maybe_unused]] inline constexpr auto upper_bound = detail::upper_bound_fn{};
	[[maybe_unused]] inline constexpr auto validate = detail::validate_fn{};
	[[maybe_unused]] inline constexpr auto insert = detail::insert_fn{};
	[[maybe_unused]] inline constexpr auto erase = detail::erase_fn{};
	using detail::forward_iter;
}

namespace std
{
	template <>
	struct hash<zero_cost_serialization::map::link>
	{
		constexpr auto operator()(zero_cost_serialization::map::link x) const noexcept
		{
			return std::size_t(x);
		}
	};
	template <typename R, typename Left, typename Right, typename Parent>
	struct hash<zero_cost_serialization::map::forward_iter<R, Left, Right, Parent>>
	{
		using is_transparent = void;

		constexpr auto operator()(const zero_cost_serialization::map::forward_iter<R, Left, Right, Parent>& it) const noexcept
		{
			return std::size_t(it.x);
		}
		constexpr auto operator()(zero_cost_serialization::map::link x) const noexcept
		{
			return std::size_t(x);
		}
	};
}
#endif
#endif
