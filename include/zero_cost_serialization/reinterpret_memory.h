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
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U& data) noexcept
	{
		ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_BEGIN
		return reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data)));
		ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_END
	}

	template <typename T, typename U, std::size_t N>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U(&data)[N]) noexcept
	{
		ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_BEGIN
		return reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data)));
		ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_END
	}

	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(std::span<U> data) noexcept
	{
		ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_BEGIN
		return reinterpret_cast<T*>(std::memmove(data.data(), data.data(), data.size_bytes()));
		ZERO_COST_SERIALIZATION_UNSAFE_BUFFER_USAGE_END
	}
}

#endif
#endif
