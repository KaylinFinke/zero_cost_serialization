#include <array>
#include <concepts>
#include <cstddef>
#include <format>
#include <iostream>
#include <span>
#include <string_view>
#include <type_traits>
#include <utility>

template <std::size_t N>
[[nodiscard]] consteval auto make_braces() noexcept
{
	using namespace std::literals::string_view_literals;
	std::array<char, N * 3 + not N> s{};
	for (std::size_t i = 0; i not_eq s.size(); ++i)
		s.at(i) = "{},"sv[i % 3];
	s.back() = {};
	return s;
}

[[nodiscard]] constexpr auto digits_n(std::integral auto t) noexcept
{
	int i{ t < decltype(t){} };
	do { t /= 10; ++i; } while (t);
	return i;
}

template <std::size_t... S>
[[nodiscard]] constexpr auto digits_pack(std::index_sequence<S...>) noexcept
{
	return (digits_n(S) + ...);
}

template <std::size_t N>
[[nodiscard]] constexpr auto digits() noexcept
{
	return N ? digits_pack(std::make_index_sequence<N>()) : std::size_t{};
}

constexpr auto to_chars(std::span<char> s, std::integral auto n) noexcept
{
	using namespace std::literals::string_view_literals;
	auto i = s.size();
	auto neg = n < 0;
	do {
		s[--i] = "0123456789"sv[n % 10];
		n /= 10;
	} while (n);
	if (neg) s[--i] = '-';
}

template <std::size_t N>
[[nodiscard]] consteval auto make_binds() noexcept
{
	std::array<char, 2 * N + not N + digits<N>()> s{};
	std::span v = std::span(s).subspan(0);
	for (auto n = std::size_t{}; n not_eq N; ++n) {
		v.front() = 'v';
		v = v.subspan(1);
		to_chars(v.first(std::size_t(digits_n(n))), n);
		v = v.subspan(std::size_t(digits_n(n)));
		v.front() = ',';
		v = v.subspan(1);
	}
	s.back() = {};
	return s;
}

template <std::size_t N>
inline constexpr auto braces_n = make_braces<N>();

template <std::size_t N>
inline constexpr auto braces = braces_n<N>.data();

int main()
{
	constexpr auto n = 255;

	std::cout << "#ifndef C17AB45F97AA4A1EAF3129E2BA17DE70" << '\n';
	std::cout << "#define C17AB45F97AA4A1EAF3129E2BA17DE70" << '\n';
	std::cout << "#ifdef C17AB45F97AA4A1EAF3129E2BA17DE70" << '\n';
	std::cout << "" << '\n';
	std::cout << "#include <cstddef>" << '\n';
	std::cout << "#include <tuple>" << '\n';
	std::cout << "#include <type_traits>" << '\n';
	std::cout << "" << '\n';
	std::cout << "namespace zero_cost_serialization::detail {" << '\n';
	std::cout << "\ttemplate <typename, std::size_t, typename = void>\n\tstruct init_n : std::false_type {};" << '\n';
	std::cout << "" << '\n';
	for (auto i = 0; i not_eq n + 1; ++i)
		std::cout << std::format("\ttemplate <typename T>\n\tstruct init_n<T, std::size_t{{{}}}, std::void_t<decltype(T({:.{}s}))>> : std::true_type {{}};\n\n", i, braces<n>, i ? i * 3 - 1 : 0);
	std::cout << "\ttemplate <typename T, typename = void>\n\tstruct make_tuple { constexpr auto operator()(T&) const noexcept\n\t{\n\t\treturn std::make_tuple();\n\t}};\n" << '\n';
	for (auto i = 0, j = digits_n(i) + 1; i not_eq n; j += digits_n(i) + 2)
		std::cout << std::format("\ttemplate <typename T>\n\tstruct make_tuple<T, std::integral_constant<std::size_t, std::size_t{{{}}}>> {{ [[maybe_unused, nodiscard]] constexpr auto operator()(T& t) const noexcept \n\t{{\n\t\tauto& [{:.{}s}] = t;\n\t\treturn std::tie({:.{}s});\n\t}}}};\n\n",
			++i, make_binds<n>().data(), j, make_binds<n>().data(), j);
	std::cout << "\ttemplate <typename T, std::size_t... Is>" << '\n';
	std::cout << "\t[[nodiscard]] consteval auto field_count(const std::index_sequence<Is...>&) noexcept" << '\n';
	std::cout << "\t{" << '\n';
	std::cout << "\t\tauto n = std::size_t{};" << '\n';
	std::cout << "\t\t((n = init_n<T, Is>::value ? Is : n), ...);" << '\n';
	std::cout << "\t\treturn n;" << '\n';
	std::cout << "\t}" << '\n';
	std::cout << "" << '\n';
	std::cout << "\ttemplate <typename T>" << '\n';
	std::cout << "\t[[nodiscard]] consteval auto field_count() noexcept" << '\n';
	std::cout << "\t{" << '\n';
	std::cout << std::format("\t\treturn field_count<T>(std::make_index_sequence<{}>());\n", n + 1);
	std::cout << "\t}" << '\n';
	std::cout << "}" << '\n';
	std::cout << "" << '\n';
	std::cout << "#endif" << '\n';
	std::cout << "#endif" << '\n';
	return 0;
}
