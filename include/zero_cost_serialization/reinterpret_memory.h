#ifndef D6397F19A531494C952ED8D656D82E24
#define D6397F19A531494C952ED8D656D82E24
#ifdef D6397F19A531494C952ED8D656D82E24

#include <cstring>
#include <new>
#include <type_traits>
#include <span>
#include "zero_cost_serialization/detail/warning.h"

namespace zero_cost_serialization {
	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>, std::bool_constant<sizeof(T) <= sizeof(U)>>
	[[nodiscard]] auto reinterpret_memory(U& data) noexcept
	{
		auto new_size = sizeof(data);
		auto new_data = static_cast<void*>(&data);
		auto _ = std::align(alignof(T), std::max(sizeof(data), sizeof(T)), new_data, new_size);
		if (new_size == sizeof(data)) {
			ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_BEGIN
			return reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data)));
			ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_END
		} else
			return static_cast<T*>(nullptr);
	}

	template <typename T, typename U, std::size_t N>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>, std::bool_constant<sizeof(T) <= sizeof(U[N])>>
	[[nodiscard]] auto reinterpret_memory(U(&data)[N]) noexcept
	{
		auto new_size = sizeof(data);
		auto new_data = static_cast<void*>(&data);
		auto _ = std::align(alignof(T), std::max(sizeof(data), sizeof(T)), new_data, new_size);
		if (new_size == sizeof(data)) {
			ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_BEGIN
			return reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data)));
			ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_END
		} else
			return static_cast<T*>(nullptr);
	}

	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(std::span<U> data) noexcept
	{
		auto new_size = data.size_bytes();
		auto new_data = static_cast<void*>(data.data());
		auto ptr = std::align(alignof(T), std::max(data.size_bytes(), sizeof(T)), new_data, new_size);
		if (ptr and new_size == data.size_bytes()) {
			ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_BEGIN
			return reinterpret_cast<T*>(std::memmove(data.data(), data.data(), data.size_bytes()));
			ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_END
		} else
			return static_cast<T*>(nullptr);
	}
}

#endif
#endif
