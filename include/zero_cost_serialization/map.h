#ifndef EF2704183C34412987AC7B604BAB84A7
#define EF2704183C34412987AC7B604BAB84A7
#ifdef EF2704183C34412987AC7B604BAB84A7

#include <optional>
#include "zero_cost_serialization/detail/platform.h"
#include "zero_cost_serialization/detail/link.h"

namespace zero_cost_serialization::map {
	using detail::color;
	using detail::link;
	namespace detail {
		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent>
		constexpr auto left_rotate(R&& rng, map::link root, map::link x, Left left, Right right, Parent parent)
		{
			const auto y = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), x, right) = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, left);
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, left))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, left, parent) = x;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, parent) = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, parent);
			if (map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, parent))
				root = y;
			else if (x == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, parent, left))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, parent, left) = y;
			else
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, parent, right) = y;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, left) = x;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), x, parent) = y;
			return root;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto left_insert_fixup(R&& rng, map::link root, map::link z, Left left, Right right, Parent parent, Color color)
		{
			const auto y = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent, parent, right);
			if (map::color::red == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), y, color)) {
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent, color) = map::color::black;
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, color) = map::color::black;
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent, parent, color) = map::color::red;
				z = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent, parent);
			} else {
				if (z == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent, right)) {
					z = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent);
					root = detail::left_rotate(std::forward<R>(rng), root, z, left, right, parent);
				}
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent, color) = map::color::black;
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent, parent, color) = map::color::red;
				root = detail::left_rotate(std::forward<R>(rng), root, zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent, parent), right, left, parent);
			}
			return std::pair{ root, z };
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto insert_fixup(R&& rng, map::link root, map::link z, Left left, Right right, Parent parent, Color color)
		{
			while (map::color::red == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), z, parent, color))
				if (zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent) == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent, parent, left))
					std::tie(root, z) = detail::left_insert_fixup(std::forward<R>(rng), root, z, left, right, parent, color);
				else
					std::tie(root, z) = detail::left_insert_fixup(std::forward<R>(rng), root, z, right, left, parent, color);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), root, color) = map::color::black;
			return root;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Right>
		constexpr auto max(R&& rng, map::link x, Right right)
		{
			while (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right))
				x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right);
			return x;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, zero_cost_serialization::detail::read_link_proj<R> Parent>
		constexpr auto successor(R&& rng, map::link x, Left left, Right right, Parent parent)
		{
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right))
				return detail::max(std::forward<R>(rng), zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right), left);
			auto y = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, parent);
			while (map::link::nil not_eq y and x == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, right)) {
				x = y;
				y = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent);
			}
			return y;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto left_delete_fixup(R&& rng, map::link root, map::link x, map::link xp, Left left, Right right, Parent parent, Color color)
		{
			auto w = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), xp, right);
			if (map::color::red == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), w, color)) {
				zero_cost_serialization::detail::ref(std::forward<R>(rng), w, color) = map::color::black;
				zero_cost_serialization::detail::ref(std::forward<R>(rng), xp, color) = map::color::red;
				root = detail::left_rotate(std::forward<R>(rng), root, xp, left, right, parent);
				w = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), xp, right);
			}
			if (map::color::black == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), w, left, color) and map::color::black == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), w, right, color)) {
				zero_cost_serialization::detail::ref(std::forward<R>(rng), w, color) = map::color::red;
				x = xp;
				xp = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, parent);
			} else {
				if (map::color::black == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), w, right, color)) {
					zero_cost_serialization::detail::ref(std::forward<R>(rng), w, left, color) = map::color::black;
					zero_cost_serialization::detail::ref(std::forward<R>(rng), w, color) = map::color::red;
					root = detail::left_rotate(std::forward<R>(rng), root, w, right, left, parent);
					w = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), xp, right);
				}
				zero_cost_serialization::detail::ref(std::forward<R>(rng), w, color) = zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), xp, color);
				zero_cost_serialization::detail::ref(std::forward<R>(rng), xp, color) = map::color::black;
				if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), w, right))
					zero_cost_serialization::detail::ref(std::forward<R>(rng), w, right, color) = map::color::black;
				x = root = detail::left_rotate(std::forward<R>(rng), root, xp, left, right, parent);
			}
			return std::tuple{ root, x, xp };
		}


		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto delete_fixup(R&& rng, map::link root, map::link x, map::link xp, Left left, Right right, Parent parent, Color color)
		{
			while (root not_eq x and map::color::black == zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), x, color))
				if (map::link::nil not_eq xp and zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), xp, left) == x)
					std::tie(root, x, xp) = detail::left_delete_fixup(std::forward<R>(rng), root, x, xp, left, right, parent, color);
				else
					std::tie(root, x, xp) = detail::left_delete_fixup(std::forward<R>(rng), root, x, xp, right, left, parent, color);
			if (map::link::nil not_eq x)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), x, color) = map::color::black;
			return root;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto transplant(R&& rng, map::link root, map::link z, map::link y, Left left, Right right, Parent parent, Color color)
		{
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent)) {
				if (zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent, left) == z)
					zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent, left) = y;
				else
					zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent, right) = y;
			}
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, left))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, left, parent) = y;
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, right))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z, right, parent) = y;
			if (root == z)
				root = y;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, color) = zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), z, color);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, left) = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, left);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, right) = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, right);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, parent) = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent);
			return root;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto relink(R&& rng, map::link root, map::link z, map::link y, Left left, Right right, Parent parent, Color color)
		{
			auto z_parent = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, parent);
			auto z_left = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, left);
			auto z_right = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, right);
			auto z_is_left = map::link::nil not_eq z_parent and zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z_parent, left) == z;

			auto y_parent = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent);
			auto y_left = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, left);
			auto y_right = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, right);
			auto y_is_left = map::link::nil not_eq y_parent and zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y_parent, left) == y;

			if (y_is_left)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_parent, left) = z;
			else if (map::link::nil not_eq y_parent)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_parent, right) = z;
			if (map::link::nil not_eq y_right)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_right, parent) = z;
			if (map::link::nil not_eq y_left)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_left, parent) = z;

			if (z_is_left)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_parent, left) = y;
			else if (map::link::nil not_eq z_parent)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_parent, right) = y;
			if (map::link::nil not_eq z_right)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_right, parent) = y;
			if (map::link::nil not_eq z_left)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), z_left, parent) = y;

			auto z_color = zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), z, color);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, color) = zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), y, color);
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, color) = z_color;

			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, left) = y_left not_eq z ? y_left : y;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, right) = y_right not_eq z ? y_right : y;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent) = y_parent not_eq z ? y_parent : y;

			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, left) = z_left not_eq y ? z_left : z;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, right) = z_right not_eq y ? z_right : z;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), y, parent) = z_parent not_eq y ? z_parent : z;

			if (z == root)
				return y;
			else if (y == root)
				return z;
			else
				return root;
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color, typename Less = std::ranges::less, typename Key = std::identity>
		constexpr auto insert(R&& rng, map::link root, map::link z, Left left, Right right, Parent parent, Color color, Less less = {}, Key key = {})
		{
			auto y = map::link::nil;
			auto x = root;
			while (map::link::nil not_eq x) {
				y = x;
				if (std::invoke(less, zero_cost_serialization::detail::ref(std::forward<R>(rng), z, key), zero_cost_serialization::detail::ref(std::forward<R>(rng), x, key)))
					x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, left);
				else
					x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right);
			}
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, color) = map::color::red;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, left) = map::link::nil;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, right) = map::link::nil;
			zero_cost_serialization::detail::ref(std::forward<R>(rng), z, parent) = y;
			if (map::link::nil == y)
				root = z;
			else if (std::invoke(less, zero_cost_serialization::detail::ref(std::forward<R>(rng), z, key), zero_cost_serialization::detail::ref(std::forward<R>(rng), y, key)))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, left) = z;
			else
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, right) = z;
			return detail::insert_fixup(std::forward<R>(rng), root, z, left, right, parent, color);
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
		constexpr auto erase(R&& rng, map::link root, map::link z, Left left, Right right, Parent parent, Color color)
		{
			auto y = z;
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, left) and map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), z, right))
				y = detail::successor(std::forward<R>(rng), z, left, right, parent);
			auto y_child = map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, left) ? zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, left) : zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, right);
			if (map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent))
				root = y_child;
			else if (y == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent, left))
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, parent, left) = y_child;
			else
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y, parent, right) = y_child;
			if (map::link::nil not_eq y_child)
				zero_cost_serialization::detail::ref(std::forward<R>(rng), y_child, parent) = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent);
			auto y_color = zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), y, color);
			auto y_child_parent = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent) == z ? y : zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), y, parent);
			if (y not_eq z)
				root = detail::transplant(std::forward<R>(rng), root, z, y, left, right, parent, color);
			if (map::color::black == y_color)
				root = detail::delete_fixup(std::forward<R>(rng), root, y_child, y_child_parent, left, right, parent, color);
			return root;
		}

		template <std::ranges::random_access_range R>
		requires std::ranges::sized_range<R>
		constexpr auto validate_index(R&& rng, map::link curr)
		{
			return map::link::nil not_eq curr and static_cast<std::size_t>(curr) <= std::forward<R>(rng).size();
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, zero_cost_serialization::detail::read_link_proj<R> Parent>
		constexpr auto validate_link(R&& rng, map::link curr, map::link next, Left left, Right right, Parent parent)
		{
			return zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, parent) == next and (map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, left) or map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, right) or zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, left) not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, right));
		}


		template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, zero_cost_serialization::detail::read_link_proj<R> Parent, zero_cost_serialization::detail::read_color_proj<R> Color>
		requires std::ranges::sized_range<R>
		constexpr auto validate_min(R&& rng, map::link curr, std::ranges::range_size_t<R> black_height, Left left, Right right, Parent parent, Color color) -> decltype(std::make_optional(std::pair{ curr, black_height }))
		{
			for (; map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, left); curr = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, left)) {
				if (not detail::validate_index(std::forward<R>(rng), zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, left)))
					return std::nullopt;
				if (not detail::validate_link(std::forward<R>(rng), zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, left), curr, left, right, parent))
					return std::nullopt;
				if (zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), curr, left, color) == map::color::black)
					++black_height;
			}
			return std::make_optional(std::pair{ curr, black_height });
		}

		template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, zero_cost_serialization::detail::read_link_proj<R> Parent, zero_cost_serialization::detail::read_color_proj<R> Color>
		requires std::ranges::sized_range<R>
		constexpr auto validate_successor(R&& rng, map::link curr, std::ranges::range_size_t<R> black_height, Left left, Right right, Parent parent, Color color) -> decltype(std::make_optional(std::pair{ curr, black_height }))
		{
			if (map::link::nil not_eq zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, right)) {
				if (not detail::validate_index(std::forward<R>(rng), zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, right)))
					return std::nullopt;
				if (not detail::validate_link(std::forward<R>(rng), zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, right), curr, left, right, parent))
					return std::nullopt;
				if (zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), curr, right, color) == map::color::black)
					++black_height;
				if (auto min = detail::validate_min(std::forward<R>(rng), zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, right), black_height, left, right, parent, color))
					std::tie(curr, black_height) = min.value();
				else
					return std::nullopt;
			} else
				for (auto done = false; not done; done = map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, parent) or zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, parent, right) not_eq curr, curr = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), curr, parent))
					if (zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), curr, color) == map::color::black)
						--black_height;
			return std::make_optional(std::pair{curr, black_height});
		}
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, zero_cost_serialization::detail::read_link_proj<R> Parent>
	requires (std::semiregular<R> and std::semiregular<Left> and std::semiregular<Right> and std::semiregular<Parent> and std::ranges::view<R> and std::ranges::sized_range<R>)
	struct forward_iter
	{
		using value_type = std::ranges::range_value_t<R>;
		using difference_type = std::ranges::range_difference_t<R>;
		using iterator_category = std::forward_iterator_tag;

		R rng;
		map::link x;
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
		friend constexpr auto operator==(map::link a, const forward_iter& b) noexcept
		{
			return a == b.x;
		}
		friend constexpr auto operator==(const forward_iter& a, map::link b) noexcept
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
	forward_iter(std::span<T, Extent> rng, map::link x, Left left, Right right, Parent parent) -> forward_iter<std::span<T, std::dynamic_extent>, Left, Right, Parent>;

	template <std::ranges::random_access_range R, typename V, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, typename Less = std::ranges::less, typename Key = std::identity>
	requires (std::indirect_strict_weak_order<Less, const V*, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
	auto upper_bound(R&& rng, map::link x, const V& value, Left left, Right right, Less less = {}, Key key = {})
	{
		auto z = map::link::nil;
		while (map::link::nil not_eq x)
			if (std::invoke(less, value, zero_cost_serialization::detail::ref(std::forward<R>(rng), x, key))) {
				z = x;
				x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, left);
			} else
				x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right);
		return z;
	}


	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left>
	constexpr auto min(R&& rng, map::link root, Left left)
	{
		return map::link::nil not_eq root ? detail::max(std::forward<R>(rng), root, left) : root;
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Right>
	constexpr auto max(R&& rng, map::link root, Right right)
	{
		return map::link::nil not_eq root ? detail::max(std::forward<R>(rng), root, right) : root;
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color, typename Less = std::ranges::less, typename Key = std::identity>
	requires (std::indirect_strict_weak_order<Less, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
	constexpr auto insert(R&& rng, map::link root, std::ranges::range_size_t<R> index, Left left, Right right, Parent parent, Color color, Less less = {}, Key key = {})
	{
		return detail::insert(std::forward<R>(rng), root, zero_cost_serialization::detail::make_link(index), left, right, parent, color, less, key);
	}


	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
	constexpr auto erase(R&& rng, map::link root, std::ranges::range_size_t<R> index, Left left, Right right, Parent parent, Color color)
	{
		return detail::erase(std::forward<R>(rng), root, zero_cost_serialization::detail::make_link(index), left, right, parent, color);
	}

	template <std::ranges::random_access_range R, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, zero_cost_serialization::detail::read_link_proj<R> Parent, zero_cost_serialization::detail::read_color_proj<R> Color, typename Less = std::ranges::less, typename Key = std::identity>
	requires (std::indirect_strict_weak_order<Less, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key> and std::ranges::sized_range<R>)
	constexpr auto validate(R&& rng, map::link root, Left left, Right right, Parent parent, Color color, Less less = {}, Key key = {})
	{
		auto black_height = std::ranges::range_size_t<R>{1};
		if (map::link::nil == root)
			return true;
		if (not detail::validate_index(std::forward<R>(rng), root))
			return false;
		if (not detail::validate_link(std::forward<R>(rng), root, map::link::nil, left, right, parent))
			return false;
		if (zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), root, color) not_eq map::color::black)
			return false;
		if (auto min = detail::validate_min(std::forward<R>(rng), root, black_height, left, right, parent, color))
			std::tie(root, black_height) = min.value();
		else
			return false;
		auto curr_height = black_height;
		for (auto prev = root; map::link::nil not_eq prev; prev = root) {
			if ((map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), root, left) or map::link::nil == zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), root, right)) and curr_height not_eq black_height)
				return false;
			if (zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), root, color) == map::color::red and zero_cost_serialization::detail::val<map::color>(std::forward<R>(rng), root, parent, color) == map::color::red)
				return false;

			if (auto next = detail::validate_successor(std::forward<R>(rng), root, curr_height, left, right, parent, color))
				std::tie(root, curr_height) = next.value();
			else
				return false;

			if (map::link::nil not_eq root and std::invoke(less, zero_cost_serialization::detail::ref(std::forward<R>(rng), root, key), zero_cost_serialization::detail::ref(std::forward<R>(rng), prev, key)))
				return false;
		}
		return true;
	}

	template <std::ranges::random_access_range R, typename V, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, typename Less = std::ranges::less, typename Key = std::identity>
	requires (std::indirect_strict_weak_order<Less, const V*, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
	auto lower_bound(R&& rng, map::link x, const V& value, Left left, Right right, Less less = {}, Key key = {})
	{
		auto z = map::link::nil;
		while (map::link::nil not_eq x)
			if (not std::invoke(less, zero_cost_serialization::detail::ref(std::forward<R>(rng), x, key), value)) {
				z = x;
				x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, left);
			} else
				x = zero_cost_serialization::detail::val<map::link>(std::forward<R>(rng), x, right);
		return z;
	}

	template <std::ranges::random_access_range R, typename V, zero_cost_serialization::detail::read_link_proj<R> Left, zero_cost_serialization::detail::read_link_proj<R> Right, typename Less = std::ranges::less, typename Key = std::identity>
	requires (std::indirect_strict_weak_order<Less, const V*, std::projected<std::ranges::iterator_t<R>, Key>> and std::copy_constructible<Less> and std::copy_constructible<Key>)
	auto find(R&& rng, map::link x, const V& value, Left left, Right right, Less less = {}, Key key = {})
	{
		x = map::lower_bound(std::forward<R>(rng), x, value, left, right, less, key);
		if (map::link::nil not_eq x and not std::invoke(less, value, zero_cost_serialization::detail::ref(std::forward<R>(rng), x, key)))
			return x;
		return map::link::nil;
	}

	// swap the in-tree nodes y and z such that y is at the position z was in the tree and vice versa.
	// DANGEROUS. does NOT swap keys/values. use this for changing which indices reference which actual
	// nodes e.g. to sort an underlying array. The user must swap the keys/values to maintain the tree.
	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
	constexpr auto relink(R&& rng, map::link root, std::ranges::range_size_t<R> z, std::ranges::range_size_t<R> y, Left left, Right right, Parent parent, Color color)
	{
		return detail::relink(std::forward<R>(rng), root, zero_cost_serialization::detail::make_link(z), zero_cost_serialization::detail::make_link(y), left, right, parent, color);
	}

	// swap the in-tree node z with the out-of-tree node y.
	// After this call z is not in the tree and y is where z was in the tree.
	// DANGEROUS. does NOT swap keys/values. use this for changing which indices reference which actual
	// nodes e.g. to compact a sparse array. The user must swap the keys/values to maintain the tree.
	template <std::ranges::random_access_range R, zero_cost_serialization::detail::write_link_proj<R> Left, zero_cost_serialization::detail::write_link_proj<R> Right, zero_cost_serialization::detail::write_link_proj<R> Parent, zero_cost_serialization::detail::write_color_proj<R> Color>
	constexpr auto transplant(R&& rng, map::link root, std::ranges::range_size_t<R> z, std::ranges::range_size_t<R> y, Left left, Right right, Parent parent, Color color)
	{
		return detail::transplant(std::forward<R>(rng), root, zero_cost_serialization::detail::make_link(z), zero_cost_serialization::detail::make_link(y), left, right, parent, color);
	}
}

namespace std
{
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
