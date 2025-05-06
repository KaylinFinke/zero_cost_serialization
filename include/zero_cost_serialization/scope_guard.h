#ifndef E7FA552B69C64F93ABD4FB1234A506F1
#define E7FA552B69C64F93ABD4FB1234A506F1
#ifdef E7FA552B69C64F93ABD4FB1234A506F1
#include "zero_cost_serialization/detail/platform.h"
#include "zero_cost_serialization/detail/warning.h"
#include "zero_cost_serialization/constraint.h"
#include "zero_cost_serialization/detail/error.h"
#include <climits>

namespace zero_cost_serialization
{
	template <typename EF>
	struct scope_exit
	{
		[[nodiscard]] explicit scope_exit(auto&& fn) 
			noexcept(std::is_nothrow_constructible_v<EF, decltype(fn)> or std::is_nothrow_constructible_v<EF, decltype(fn)&>)
			requires(not std::is_same_v<std::remove_cvref_t<decltype(fn)>, scope_exit> and std::is_constructible_v<EF, decltype(fn)>)
			ZERO_COST_SERIALIZATION_TRY
			: exitfun{zero_cost_serialization::forward_if_noexcept<EF>(std::forward<decltype(fn)>(fn))}
		{}
		ZERO_COST_SERIALIZATION_CATCH(..., { fn(); if constexpr (not (std::is_nothrow_constructible_v<EF, decltype(fn)> or std::is_nothrow_constructible_v<EF, decltype(fn)&>)) throw; })

		[[nodiscard]] scope_exit(scope_exit&& other)
			noexcept(std::is_nothrow_move_constructible_v<EF> or std::is_nothrow_copy_constructible_v<EF>)
			requires(std::is_nothrow_move_constructible_v<EF> or std::is_copy_constructible_v<EF>)
			: exitfun{zero_cost_serialization::forward_if_noexcept<EF>(std::forward<EF>(other.exitfun))}
		{
			other.release();
		}

		decltype(auto) operator=(scope_exit&&) = delete;

		auto release() noexcept
		{
			enabled = {};
		}

		~scope_exit() noexcept
		{
			if (enabled)
				exitfun();
		}

	private:
		ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS EF exitfun;
		bool enabled = { true };
	};

	template <typename EF>
	scope_exit(EF) -> scope_exit<EF>;

	template <typename EF>
	struct scope_fail
	{
		[[nodiscard]] explicit scope_fail(auto&& fn)
			noexcept(std::is_nothrow_constructible_v<EF, decltype(fn)> or std::is_nothrow_constructible_v<EF, decltype(fn)&>)
			requires(not std::is_same_v<std::remove_cvref_t<decltype(fn)>, scope_fail> and std::is_constructible_v<EF, decltype(fn)>)
		ZERO_COST_SERIALIZATION_TRY
			: exitfun{zero_cost_serialization::forward_if_noexcept<EF>(std::forward<decltype(fn)>(fn))}
			, enabled{ unsigned(INT_MIN) | unsigned(std::uncaught_exceptions()) }
		{}
		ZERO_COST_SERIALIZATION_CATCH(..., { fn(); if constexpr (not (std::is_nothrow_constructible_v<EF, decltype(fn)> or std::is_nothrow_constructible_v<EF, decltype(fn)&>)) throw; })

		[[nodiscard]] scope_fail(scope_fail&& other)
			noexcept(std::is_nothrow_move_constructible_v<EF> or std::is_nothrow_copy_constructible_v<EF>)
			requires(std::is_nothrow_move_constructible_v<EF> or std::is_copy_constructible_v<EF>)
			: exitfun{zero_cost_serialization::forward_if_noexcept<EF>(std::forward<EF>(other.exitfun))}
			, enabled{ unsigned(INT_MIN) | unsigned(std::uncaught_exceptions()) }
		{
			other.release();
		}

		decltype(auto) operator=(scope_fail&&) = delete;

		auto release() noexcept
		{
			enabled &= unsigned(INT_MAX);
		}

		~scope_fail() noexcept
		{
			if ((unsigned(INT_MIN) & enabled) and (unsigned(INT_MAX) & enabled) < unsigned(std::uncaught_exceptions()))
				exitfun();
		}

	private:
		ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS EF exitfun;
		unsigned enabled;
	};

	template <typename EF>
	scope_fail(EF) -> scope_fail<EF>;

	template <typename EF>
	struct scope_success
	{
		[[nodiscard]] explicit scope_success(auto&& fn)
			noexcept(std::is_nothrow_constructible_v<EF, decltype(fn)> or std::is_nothrow_constructible_v<EF, decltype(fn)&>)
			requires(not std::is_same_v<std::remove_cvref_t<decltype(fn)>, scope_success> and std::is_constructible_v<EF, decltype(fn)>)
			: exitfun{zero_cost_serialization::forward_if_noexcept<EF>(std::forward<decltype(fn)>(fn))}
			, enabled{ unsigned(INT_MIN) | unsigned(std::uncaught_exceptions()) }
		{}

		[[nodiscard]] scope_success(scope_success&& other)
			noexcept(std::is_nothrow_move_constructible_v<EF> or std::is_nothrow_copy_constructible_v<EF>)
			requires(std::is_nothrow_move_constructible_v<EF> or std::is_copy_constructible_v<EF>)
			: exitfun{zero_cost_serialization::forward_if_noexcept<EF>(std::forward<EF>(other.exitfun))}
			, enabled{ unsigned(INT_MIN) | unsigned(std::uncaught_exceptions()) }
		{
			other.release();
		}

		decltype(auto) operator=(scope_success&&) = delete;

		auto release() noexcept
		{
			enabled &= unsigned(INT_MAX);
		}

		~scope_success() noexcept(noexcept(exitfun()))
		{
			if ((unsigned(INT_MIN) & enabled) and (unsigned(INT_MAX) & enabled) >= unsigned(std::uncaught_exceptions()))
				exitfun();
		}

	private:
		ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS EF exitfun;
		unsigned enabled;
	};

	template <typename EF>
	scope_success(EF) -> scope_success<EF>;

	namespace detail
	{
		template <typename T>
		struct holder
		{
			holder() = default;

			template <typename OnFail>
			holder(auto&& x, zero_cost_serialization::scope_fail<OnFail>)
				: value{ zero_cost_serialization::forward_if_noexcept<T>(std::forward<decltype(x)>(x)) }
			{}

			ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS T value = {};
		};

		template <typename T, typename U>
		holder(T, U) -> holder<T>;
	}

	template <typename R, typename D>
	class unique_resource
	{
		using RS = std::conditional_t<std::is_object_v<R>, R, std::reference_wrapper<std::remove_reference_t<R>>>;
		ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS detail::holder<RS> resource = {};
		ZERO_COST_SERIALIZATION_NO_UNIQUE_ADDRESS detail::holder<D> deleter = {};
		bool enabled = {};

	public:
		unique_resource() = default;

		unique_resource(auto&& r, auto&& d)
			noexcept((std::is_nothrow_constructible_v<RS, decltype(r)> or std::is_nothrow_constructible_v<RS, decltype(r)&>)
				and (std::is_nothrow_constructible_v<D, decltype(d)> or std::is_nothrow_constructible_v<D, decltype(d)&>))
			requires(std::is_constructible_v<RS, decltype(r)> and std::is_constructible_v<D, decltype(d)>
				and (std::is_nothrow_constructible_v<RS, decltype(r)> or std::is_constructible_v<RS, decltype(r)&>)
				and (std::is_nothrow_constructible_v<D, decltype(d)> or std::is_constructible_v<D, decltype(d)&>))
			: resource{ std::forward<decltype(r)>(r), zero_cost_serialization::scope_fail{[&] { d(r); }} }
ZERO_COST_SERIALIZATION_THIS_USED_IN_MEMBER_INIT
			, deleter{ std::forward<decltype(d)>(d), zero_cost_serialization::scope_fail{[&,this] { d(get()); }} }
			, enabled{true}
			{}

		unique_resource(unique_resource&& other)
			noexcept(std::is_nothrow_move_constructible_v<RS> and std::is_nothrow_move_constructible_v<D>)
			: resource{ std::move_if_noexcept<RS>(other.resource.value), zero_cost_serialization::scope_fail{[]{}}}
			, deleter{ std::move_if_noexcept<D>(other.deleter.value), zero_cost_serialization::scope_fail{[&]
				{ 
					if constexpr (std::is_nothrow_move_constructible_v<RS>)
						if (other.enabled) {
							other.release();
							other.get_deleter()(get());
						}
				}}}
			, enabled{std::exchange(other.enabled, false)}
		{}

		decltype(auto) operator=(unique_resource&& other) noexcept(std::is_nothrow_move_assignable_v<RS> and std::is_nothrow_move_assignable_v<D>)
		{
			if (this != &other)
			{
				reset();

				if constexpr (std::is_nothrow_move_assignable_v<D> or not std::is_nothrow_move_assignable_v<RS>) {
					if constexpr (std::is_nothrow_move_assignable_v<RS>)
						resource = std::move(other.resource);
					else
						resource = other.resource;
				}
				if constexpr (std::is_nothrow_move_assignable_v<D>)
					deleter = std::move(other.deleter);
				else
					deleter = other.deleter;

				if constexpr (not std::is_nothrow_move_assignable_v<D> and std::is_nothrow_move_assignable_v<RS>)
					resource = std::move(other.resource);
				
				enabled = std::exchange(other.enabled, false);
			}
			return *this;
		}

		auto release() noexcept
		{
			enabled = {};
		}

		auto reset() noexcept
		{
			if (enabled) {
				release();
				get_deleter()(get());
			}
		}

		auto reset(auto&& r)
		{
			reset();
			auto _ = zero_cost_serialization::scope_fail{[&,this]{get_deleter()(r);}};
			if constexpr (std::is_nothrow_assignable_v<RS, decltype(r)>)
				resource = std::forward<decltype(r)>(r);
			else
				resource = std::as_const(r);
			enabled = { true };			
		}

		decltype(auto) get() const noexcept
		{
			if constexpr (std::is_object_v<R>)
				return (resource.value);
			else
				return std::as_const(resource.value.get());
		}

		decltype(auto) get_deleter() const noexcept
		{
			return (deleter.value);
		}

		decltype(auto) operator*() const noexcept requires(std::is_pointer_v<R> and not std::is_void_v<std::remove_pointer_t<R>>)
		{
			return *get();
		}

		auto operator->() const noexcept requires(std::is_pointer_v<R>)
		{
			return get();
		}

		~unique_resource() noexcept
		{
			reset();
		}
	};

	template <typename R, typename D>
	unique_resource(R, D) -> unique_resource<R, D>;
}

#endif
#endif
