#ifndef cargo_h
#define cargo_h

#include <strong_type/strong_type.h>
#include <type_name/type_name.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <cstring>
#include <functional>
#include <iterator>
#include <new>
#include <string_view>

namespace cargo
{
template <typename T>
struct is_seq;

template <typename T, typename Size>
class seq;

namespace details
{
struct data_info;
using DataInfoCPtrT = data_info const *;

using ValueDestructor = void (*)(char *&) noexcept;
using ValuePtrGetter = char *(*)(char *&) noexcept;
using CValuePtrGetter = char const *(*)(char const *&) noexcept;

using Offset = strong::strong_type<struct OffsetTag, uintptr_t,
                                   strong::convertible_to_bool>;
using Count = strong::strong_type<struct CountTag, std::size_t,
                                  strong::convertible_to_bool>;

template <typename T>
struct is_nothrow_copyable
    : std::disjunction<
          std::is_nothrow_copy_constructible<T>,
          std::conjunction<std::is_nothrow_default_constructible<T>,
                           std::is_nothrow_copy_assignable<T>>>
{
};

template <typename T>
constexpr inline bool is_nothrow_copyable_v = is_nothrow_copyable<T>::value;

template <typename T>
struct is_nothrow_copyable_seq : std::false_type
{
};

template <typename T, typename S>
struct is_nothrow_copyable_seq<seq<T, S>>
    : std::bool_constant<is_nothrow_copyable_v<T>>
{
};

template <typename T>
constexpr inline bool is_nothrow_copyable_seq_v =
    is_nothrow_copyable_seq<T>::value;

template <typename T>
struct is_reference_wrapper : std::false_type
{
};

template <typename T>
struct is_reference_wrapper<std::reference_wrapper<T>> : std::true_type
{
};

template <typename T>
constexpr inline bool is_reference_wrapper_v = is_reference_wrapper<T>::value;

template <typename T>
struct is_alterable : std::false_type
{
};

template <typename T>
struct is_alterable<std::reference_wrapper<T>> : std::negation<std::is_const<T>>
{
};

template <typename T>
struct is_alterable<T *> : std::negation<std::is_const<T>>
{
};

template <typename T>
struct is_alterable<T &> : std::negation<std::is_const<T>>
{
};

template <typename T, typename S>
struct is_alterable<seq<T, S>> : std::negation<std::is_const<T>>
{
};

template <typename T>
constexpr inline bool is_alterable_v = is_alterable<T>::value;

template <typename T>
struct is_value
    : std::conjunction<std::negation<std::is_reference<T>>,
                       std::negation<std::is_pointer<T>>>
{
};

template <typename T>
constexpr inline bool is_value_v = is_value<T>::value;

template <typename T>
struct is_value_without_nothrow_copy_constructible
    : std::conjunction<is_value<T>,
                       std::negation<std::is_nothrow_copy_constructible<
                           std::remove_const_t<T>>>>
{
};

template <typename T>
constexpr inline bool is_value_without_nothrow_copy_constructible_v =
    is_value_without_nothrow_copy_constructible<T>::value;

template <typename T>
struct alterable
{
    using type = utils::remove_cvref_t<T>;
};

template <typename T>
struct alterable<std::reference_wrapper<T>>
{
    using type = std::reference_wrapper<std::remove_const_t<T>>;
};

template <typename T>
struct alterable<T *>
{
    using type = std::remove_const_t<T> *;
};

template <typename T, typename S>
struct alterable<seq<T, S>>
{
    using type = seq<std::remove_const_t<T>, S>;
};

template <typename T>
using alterable_t = typename alterable<T>::type;

template <typename T>
struct stored_impl
{
    using type = T;
};

template <typename T, typename S>
struct stored_impl<seq<T, S>>
{
    using type = seq<std::remove_const_t<T>, S>;
};

template <typename T>
struct stored
{
    using type = typename stored_impl<utils::remove_cvref_t<T>>::type;
};

template <typename T>
using stored_t = typename stored<T>::type;

template <typename T, typename CharT,
          typename = std::enable_if_t<
              std::is_same_v<std::remove_const_t<CharT>, char>>>
class info_to_store;

template <typename ParamsTList, typename CharT>
struct is_valid_params;

template <typename ParamsTList>
struct is_valid_params<ParamsTList, char>
{
    template <typename T>
    struct predicate
        : std::negation<std::disjunction<
              std::is_rvalue_reference<T>,
              is_value_without_nothrow_copy_constructible<T>,
              std::conjunction<std::is_lvalue_reference<T>,
                               is_seq<utils::remove_cvref_t<T>>>>>
    {
    };
    static constexpr bool value =
        ParamsTList::size ==
        ParamsTList::template count_of_predicate_compliant<predicate>;
};

template <typename ParamsTList>
struct is_valid_params<ParamsTList, const char>
{
    template <typename T>
    struct predicate
        : std::negation<std::disjunction<
              std::is_rvalue_reference<T>, is_alterable<T>,
              is_value_without_nothrow_copy_constructible<T>,
              std::conjunction<std::is_lvalue_reference<T>,
                               is_seq<utils::remove_cvref_t<T>>>>>
    {
    };
    static constexpr bool value =
        ParamsTList::size ==
        ParamsTList::template count_of_predicate_compliant<predicate>;
};

template <typename TypeList, typename CharT>
constexpr inline bool is_valid_params_v =
    is_valid_params<TypeList, CharT>::value;

template <typename T>
constexpr bool match(char *, const std::string_view &aActualType) noexcept
{
    if constexpr (is_valid_params_v<utils::type_list<T>, char>)
    {
        const bool r = &type_name::kName<alterable_t<T>> == &aActualType;
        if constexpr (!is_alterable_v<T>)
        {
            return r || (&type_name::kName<utils::remove_cvref_t<T>> ==
                         &aActualType);
        }
        else
        {
            return r;
        }
    }
    else
    {
        return false;
    }
}

template <typename T>
constexpr bool match(char const *, const std::string_view &aActualType) noexcept
{
    if constexpr (is_valid_params_v<utils::type_list<T>, char const>)
    {
        const bool r =
            &type_name::kName<utils::remove_cvref_t<T>> == &aActualType;
        if constexpr (std::is_pointer_v<T> || is_reference_wrapper_v<T> ||
                      is_seq<T>::value)
        {
            return r || (&type_name::kName<alterable_t<T>> == &aActualType);
        }
        else
        {
            return r;
        }
    }
    else
    {
        return false;
    }
}

template <typename T>
static void store(T &&aArg, char *aValuePtr) noexcept;

template <typename T, typename CharT, typename>
class info_to_store
{
    template <typename U, typename V, typename>
    friend class info_to_store;

    constexpr info_to_store(CharT *&aPtr, Offset aOffset, Count aCount) noexcept
        : mem_size_(aOffset.get() + sizeof(T) * aCount.get())
        , data_ptr_(aPtr + aOffset.get())
    {
        assert(aPtr && "invalid aPtr");
        assert(aCount && "invalid aCount");
        aPtr += mem_size_;
    }

    constexpr info_to_store(CharT *&aPtr, const Count aCount) noexcept
        : info_to_store(aPtr, Offset(utils::skip_to_align<T>(aPtr)), aCount)
    {
    }

    constexpr static info_to_store safe_make(CharT *&aPtr,
                                             const Count aCount) noexcept
    {
        return aCount ? info_to_store(aPtr, aCount) : info_to_store();
    }

   public:
    constexpr info_to_store() = default;
    void destroy() noexcept { value_pointer()->~T(); }

    decltype(auto) value_pointer() noexcept
    {
        if constexpr (std::is_const_v<CharT>)
        {
            return reinterpret_cast<T const *>(data_ptr_);
        }
        else
        {
            return reinterpret_cast<T *>(data_ptr_);
        }
    }

    decltype(auto) value() noexcept
    {
        if constexpr (is_reference_wrapper_v<T>)
        {
            using DataT = typename T::type;
            return std::reference_wrapper<DataT>(
                *reinterpret_cast<DataT *>(data_ptr_));
        }
        else
        {
            auto &ref = *value_pointer();
            return ref;
        }
    }

    constexpr CharT *value_begin_ptr() noexcept { return data_ptr_; }

    constexpr info_to_store(CharT *&aPtr, const T &) noexcept
        : info_to_store(aPtr, Offset(utils::skip_to_align<T>(aPtr)), Count(1))
    {
    }

    constexpr info_to_store(CharT *&aPtr) noexcept
        : info_to_store(aPtr, Offset(utils::skip_to_align<T>(aPtr)), Count(1))
    {
    }

    template <typename U>
    void write(U &&aArg) noexcept
    {
        static_assert(std::is_same_v<utils::remove_cvref_t<U>, T>);
        store(std::forward<U>(aArg), data_ptr_);
    }

    constexpr std::size_t size() const noexcept { return mem_size_; }

   private:
    std::size_t mem_size_{};
    CharT *data_ptr_{};
};

template <typename T, typename Size, typename CharT>
class info_to_store<seq<T, Size>, CharT>
{
   public:
    using value_type = T;
    using size_type = Size;
    using seq_type = seq<value_type, size_type>;
    using size_info_t = info_to_store<size_type, CharT>;
    using value_info_t = info_to_store<value_type, CharT>;

    void destroy() noexcept
    {
        const auto s = *size_info_.value_pointer();
        size_info_.destroy();

        if constexpr (not std::is_same_v<value_type, char>)
        {
            value_type *value_ptr = values_info_.value_pointer();
            for (std::size_t i = 0; i < s; ++i, ++value_ptr)
            {
                value_ptr->~value_type();
            }
        }
    }

    constexpr seq_type value() noexcept
    {
        return seq_type(values_info_.value_pointer(),
                        *size_info_.value_pointer());
    }

    constexpr CharT *value_begin_ptr() noexcept
    {
        return size_info_.value_begin_ptr();
    }

    template <typename Seq>
    constexpr info_to_store(CharT *&aPtr, const Seq &aSeq) noexcept
        : size_info_(aPtr)
        , values_info_(value_info_t::safe_make(aPtr, Count(aSeq.size())))
    {
    }

    constexpr info_to_store(CharT *&aPtr) noexcept
        : size_info_(aPtr)
        , values_info_(
              value_info_t::safe_make(aPtr, Count(*size_info_.value_pointer())))
    {
    }

    template <typename U>
    void write(U &&aArg) noexcept
    {
        static_assert(std::is_same_v<stored_t<U>, seq_type>);
        const auto count = aArg.size();
        store(count, size_info_.data_ptr_);
        if (count)
        {
            if constexpr (std::is_same_v<std::remove_cv_t<value_type>, char>)
            {
                std::memcpy(values_info_.data_ptr_, aArg.data(), count);
            }
            else
            {
                char *dst_ptr = values_info_.data_ptr_;
                value_type const *value_ptr = aArg.data();
                for (std::size_t i = 0; i < count; ++i)
                {
                    store(*value_ptr, dst_ptr);
                    ++value_ptr;
                    dst_ptr += sizeof(value_type);
                }
            }
        }
    }

    constexpr std::size_t size() const noexcept
    {
        return size_info_.mem_size_ + values_info_.mem_size_;
    }

   private:
    size_info_t size_info_{};
    value_info_t values_info_{};
};

template <typename T, typename CharT>
class stored_info;

struct data_info
{
    template <typename T>
    constexpr data_info(utils::tag_t<T>) noexcept
        : type_name{type_name::kName<T>}
        , pointer_to_value(&stored_info<T, char>::value_begin_ptr)
        , pointer_to_const_value(&stored_info<T, const char>::value_begin_ptr)
        , destroy_value{&stored_info<T, char>::destroy_value}
    {
    }

    const std::string_view &type_name;
    ValuePtrGetter pointer_to_value{nullptr};
    CValuePtrGetter pointer_to_const_value{nullptr};
    ValueDestructor destroy_value{nullptr};
};

static void destroy_next(char *&aPtr) noexcept
{
    DataInfoCPtrT info =
        *info_to_store<DataInfoCPtrT, char>(aPtr).value_pointer();
    info->destroy_value(aPtr);
    info->~data_info();
}

template <typename ValueInfo, typename T>
static std::enable_if_t<
    std::disjunction_v<std::is_same<T, char>, std::is_same<T, char const>>,
    ValueInfo>
next_info(T *&aPtr) noexcept
{
    auto td = *info_to_store<DataInfoCPtrT, T>(aPtr).value_pointer();
    if constexpr (std::is_same_v<T, char>)
    {
        return {std::cref(td->type_name), td->pointer_to_value(aPtr)};
    }
    else
    {
        return {std::cref(td->type_name), td->pointer_to_const_value(aPtr)};
    }
}

template <typename T>
constexpr inline data_info kDataInfo = data_info(utils::tag<T>);

template <typename T, typename CharT>
class stored_info
{
   public:
    template <typename U>
    stored_info(CharT *&aPtr, const U &aValue) noexcept
        : type_info_(aPtr, &kDataInfo<T>), value_info_(aPtr, aValue)
    {
    }

    stored_info(CharT *&aPtr) noexcept : type_info_(aPtr), value_info_(aPtr) {}

    void destroy() noexcept
    {
        type_info_.destroy();
        value_info_.destroy();
    }

    static void destroy_value(CharT *&aPtr) noexcept
    {
        info_to_store<T, CharT>(aPtr).destroy();
    }

    static CharT *value_begin_ptr(CharT *&aPtr) noexcept
    {
        return info_to_store<T, CharT>(aPtr).value_begin_ptr();
    }

    template <typename U>
    void write(U &&aArg) noexcept
    {
        static_assert(std::is_same_v<stored_t<U>, T>);
        type_info_.write(&kDataInfo<stored_t<T>>);
        value_info_.write(std::forward<U>(aArg));
    }

    constexpr std::size_t size() const noexcept
    {
        return type_info_.size() + value_info_.size();
    }

   private:
    info_to_store<DataInfoCPtrT, CharT> type_info_;
    info_to_store<T, CharT> value_info_;
};

template <typename T>
static void store(T &&aArg, char *aValuePtr) noexcept
{
    using StoredT = utils::remove_cvref_t<T>;
    if constexpr (is_reference_wrapper_v<StoredT>)
    {  // std::reference_wrapper
        store<std::add_pointer_t<typename StoredT::type>>(&aArg.get(),
                                                          aValuePtr);
    }
    else if constexpr (std::is_pointer_v<T>)
    {  // pointer
        new (aValuePtr) StoredT(std::forward<T>(aArg));
    }
    else if constexpr (std::is_lvalue_reference_v<T>)
    {  // lvalue
        if constexpr (std::is_nothrow_copy_constructible_v<StoredT>)
        {
            new (aValuePtr) StoredT(std::forward<T>(aArg));
        }
        else if constexpr (std::is_nothrow_default_constructible_v<StoredT> &&
                           std::is_nothrow_copy_assignable_v<StoredT>)
        {
            auto p = new (aValuePtr) StoredT();
            p->operator=(std::forward<T>(aArg));
        }
    }
    else
    {  // rvalue or value
        if constexpr (std::is_nothrow_move_constructible_v<StoredT>)
        {
            new (aValuePtr) StoredT(std::forward<T>(aArg));
        }
        else if constexpr (std::is_nothrow_copy_constructible_v<StoredT>)
        {
            new (aValuePtr) StoredT(aArg);
        }
        else if constexpr (std::is_nothrow_default_constructible_v<StoredT> &&
                           std::is_nothrow_move_assignable_v<StoredT>)
        {
            auto p = new (aValuePtr) StoredT();
            p->operator=(std::forward<T>(aArg));
        }
        else if constexpr (std::is_nothrow_default_constructible_v<StoredT> &&
                           std::is_nothrow_copy_assignable_v<StoredT>)
        {
            auto p = new (aValuePtr) StoredT();
            p->operator=(aArg);
        }
    }
}

template <typename ValueInfo, typename TList, std::size_t... I>
bool match_impl(typename ValueInfo::type *aData,
                std::index_sequence<I...>) noexcept
{
    return (... && next_info<ValueInfo>(aData)
                       .template match<typename TList::template at<I>>());
}

template <typename ValueInfo, typename TList>
std::enable_if_t<utils::is_type_list_v<TList>, bool> match(
    typename ValueInfo::type *aData, std::size_t aValueCount) noexcept
{
    using CharT = typename ValueInfo::type;
    static_assert(std::is_same_v<std::remove_const_t<CharT>, char>);
    bool retVal{};
    if (TList::size <= aValueCount)
    {
        using Indices = std::make_index_sequence<TList::size>;
        retVal = match_impl<ValueInfo, TList>(aData, Indices{});
    }
    return retVal;
}

template <typename ValueInfo, typename... Ts>
std::enable_if_t<not utils::is_type_list_v<Ts...>, bool> match(
    typename ValueInfo::type *aData, std::size_t aValueCount) noexcept
{
    return match<ValueInfo, utils::type_list<Ts...>>(aData, aValueCount);
}

template <typename ValueInfo, typename ArgTypes, typename Callback,
          std::size_t... I>
void make_call(typename ValueInfo::type *aData, Callback &&aCallback,
               std::index_sequence<I...>) noexcept
{
    using CharT = typename ValueInfo::type;
    CharT *ptr[1]{aData};
    ValueInfo infos[sizeof...(I)] = {next_info<ValueInfo>(ptr[I - I])...};
    aCallback(infos[I].template get<typename ArgTypes::template at<I>>()...);
}

template <typename ValueInfo, typename Callback>
bool call_if_match(typename ValueInfo::type *aData, std::size_t aValueCount,
                   Callback &&aCallback) noexcept
{
    using CharT = typename ValueInfo::type;
    using namespace utils::function_traits;

    using Invocable = std::remove_pointer_t<utils::remove_cvref_t<Callback>>;
    static_assert(is_invocable_v<Invocable>);

    using Signature = signature_t<Invocable>;
    static_assert(has_noexcept_qualifier_v<Signature>);

    using R = return_type_t<Signature>;
    static_assert(std::is_same_v<R, void>);

    using ParamsType = params_list_t<Signature>;
    bool is_called{};
    if constexpr (is_valid_params_v<ParamsType, CharT>)
    {
        if (match<ValueInfo, ParamsType>(aData, aValueCount))
        {
            using Indices = std::make_index_sequence<ParamsType::size>;
            make_call<ValueInfo, ParamsType>(
                aData, std::forward<Callback>(aCallback), Indices{});
            is_called = true;
        }
    }
    return is_called;
}

template <typename ValueInfo, typename... Callbacks>
std::enable_if_t<not utils::is_tuple_v<utils::remove_cvref_t<Callbacks>...>,
                 bool>
process(typename ValueInfo::type *aData, std::size_t aValueCount,
        Callbacks &&...aCallbacks) noexcept
{
    return (... ||
            call_if_match<ValueInfo>(aData, aValueCount,
                                     std::forward<Callbacks>(aCallbacks)));
}

template <typename ValueInfo, typename Callbacks, std::size_t... I>
bool process_impl(typename ValueInfo::type *aData, std::size_t aValueCount,
                  Callbacks &&aCallbacks, std::index_sequence<I...>) noexcept
{
    return process<ValueInfo>(aData, aValueCount, std::get<I>(aCallbacks)...);
}

template <typename ValueInfo, typename Callbacks>
std::enable_if_t<utils::is_tuple_v<utils::remove_cvref_t<Callbacks>>, bool>
process(typename ValueInfo::type *aData, std::size_t aValueCount,
        Callbacks &&aCallbacks) noexcept
{
    using Indices = std::make_index_sequence<
        std::tuple_size_v<utils::remove_cvref_t<Callbacks>>>;
    return process_impl<ValueInfo>(
        aData, aValueCount, std::forward<Callbacks>(aCallbacks), Indices{});
}
}  // namespace details

template <typename T, typename Size>
class seq final
{
   public:
    using value_type = std::enable_if_t<not is_seq<T>::value, T>;
    using size_type = std::enable_if_t<std::is_unsigned_v<Size>, Size>;

    friend constexpr bool operator==(const seq &aLhs, const seq &aRhs) noexcept
    {
        if (aLhs.size_ != aRhs.size_)
        {
            return false;
        }

        if ((aLhs.size_ == 0) && (aRhs.size_ == 0))
        {
            return true;
        }

        if constexpr (std::has_unique_object_representations_v<
                          std::remove_const_t<value_type>> ||
                      std::is_floating_point_v<std::remove_const_t<value_type>>)
        {
            return !std::memcmp(aLhs.data(), aRhs.data(),
                                aLhs.size() * sizeof(value_type));
        }
        else
        {
            const auto is_equal = [&aLhs, &aRhs]() -> bool
            {
                std::size_t i = 0;
                while ((i < aLhs.size_) &&
                       (*(aLhs.data_ + i) == *(aRhs.data_ + i)))
                {
                    ++i;
                }
                return i == aLhs.size_;
            };
            return (aLhs.data_ == aRhs.data_) || is_equal();
        }
    }

    friend constexpr bool operator!=(const seq &aLhs, const seq &aRhs) noexcept
    {
        return !(aLhs == aRhs);
    }

    constexpr seq() noexcept = default;

    seq(const seq &) noexcept = default;
    seq &operator=(const seq &) noexcept = default;

    seq(seq &&) noexcept = default;
    seq &operator=(seq &&) noexcept = default;

    constexpr seq(value_type *aData, size_type aSize) noexcept
        : data_{aSize ? aData : nullptr}
        , size_{static_cast<size_type>(aData ? aSize : 0)}
    {
        static_assert(std::is_unsigned_v<size_type>);
        assert(((data_ && size_) || (!data_ && !size_)) &&
               "members has been initialized to invalid values");
    }

    template <typename U,
              typename V = std::enable_if_t<
                  std::is_pointer_v<utils::remove_cvref_t<U>> &&
                      std::is_same_v<std::remove_cv_t<std::remove_pointer_t<
                                         utils::remove_cvref_t<U>>>,
                                     char>,
                  U>>
    seq(U &&aData) noexcept
        : seq(aData, aData ? static_cast<size_type>(std::strlen(aData) + 1) : 0)
    {
    }

    template <Size N>
    constexpr seq(T (&aData)[N]) noexcept : seq(aData, N)
    {
    }

    inline constexpr size_type size() const noexcept { return size_; }

    inline constexpr value_type const *data() const noexcept { return data_; }

    inline constexpr value_type *data() noexcept { return data_; }

   private:
    value_type *data_{};
    size_type size_{};
};

template <typename T>
struct is_seq : std::false_type
{
};

template <typename T, typename Size>
struct is_seq<seq<T, Size>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_seq_v = is_seq<T>::value;

template <typename T>
struct value_info
{
   public:
    using type = T;
    std::reference_wrapper<const std::string_view> type_name;
    T *value{};

    template <typename U>
    constexpr bool match() const noexcept
    {
        return details::match<U>(value, type_name.get());
    }

    template <typename R>
    decltype(auto) get() noexcept
    {
        assert(details::match<R>(value, type_name.get()) &&
               "retrieved type does not match stored type");
        auto value_ptr_copy = value;
        details::info_to_store<utils::remove_cvref_t<R>,
                               std::remove_pointer_t<T>>
            inf(value_ptr_copy);
        return inf.value();
    }
};

template <typename T>
struct is_value_info : std::false_type
{
};

template <typename T>
struct is_value_info<value_info<T>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_value_info_v = is_value_info<T>::value;

using const_info = value_info<char const>;
using info = value_info<char>;

class payload
{
   private:
    template <typename T>
    friend class iterator;

    static constexpr std::string_view empty_sv{};

    template <typename T>
    class data_iterator
    {
       private:
        using CharT =
            std::enable_if_t<std::disjunction_v<std::is_same<T, info>,
                                                std::is_same<T, const_info>>,
                             typename T::type>;
        static T empty() noexcept
        {
            static constexpr std::string_view empty_sv;
            return {empty_sv, nullptr};
        }

       public:
        using iterator_category = std::input_iterator_tag;
        using difference_type = std::size_t;
        using value_type = T;
        using pointer = T *;
        using reference = T &;

        constexpr data_iterator(const payload &aPayload, CharT *aData,
                                std::size_t aIndex, T &&aInfo) noexcept
            : info_(aInfo), payload_(aPayload), data_(aData), index_(aIndex)
        {
        }

        static constexpr decltype(auto) make_begin(
            const payload &aPayload) noexcept
        {
            using details::next_info;
            CharT *data_ptr = aPayload.data_;
            auto val_inf = (aPayload.space_used_ > 0)
                               ? next_info<value_type>(data_ptr)
                               : empty();
            return data_iterator<T>(aPayload, data_ptr, 0, std::move(val_inf));
        }

        static constexpr decltype(auto) make_end(
            const payload &aPayload) noexcept
        {
            char *data_ptr = aPayload.data_ + aPayload.space_used_;
            return data_iterator<T>(aPayload, data_ptr, aPayload.size_,
                                    empty());
        }

        reference operator*() noexcept { return info_; }

        pointer operator->() noexcept { return &info_; }

        data_iterator &operator++() noexcept
        {
            using details::next_info;
            if (++index_ < payload_.size_)
            {
                info_ = next_info<value_type>(data_);
            }
            else
            {
                index_ = payload_.size_;
                info_ = empty();
            }
            return *this;
        }

        data_iterator operator++(int) noexcept
        {
            data_iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        friend bool operator==(const data_iterator &a,
                               const data_iterator &b) noexcept
        {
            return (a.index_ == b.index_) && (&a.payload_ == &b.payload_);
        };
        friend bool operator!=(const data_iterator &a,
                               const data_iterator &b) noexcept
        {
            return (a.index_ != b.index_) || (&a.payload_ != &b.payload_);
        };

       private:
        T info_;
        const payload &payload_;
        CharT *data_;
        std::size_t index_{};
    };

   public:
    using iterator = data_iterator<info>;
    using const_iterator = data_iterator<const_info>;

    iterator begin() noexcept { return iterator::make_begin(*this); }

    iterator end() noexcept { return iterator::make_end(*this); }

    const_iterator cbegin() const noexcept
    {
        return const_iterator::make_begin(*this);
    }

    const_iterator cend() const noexcept
    {
        return const_iterator::make_end(*this);
    }

    ~payload() { reset(); }

    constexpr payload(payload &&aOther) noexcept
        : data_{aOther.data_}
        , capacity_{aOther.capacity_}
        , size_{aOther.size_}
        , space_used_{aOther.space_used_}
        , space_shortage_{aOther.space_shortage_}
    {
        aOther.data_ = nullptr;
        aOther.capacity_ = 0;
        aOther.size_ = 0;
        aOther.space_used_ = 0;
        aOther.space_shortage_ = 0;
    }

    payload &operator=(payload &&aOther) noexcept
    {
        if (this != &aOther)
        {
            this->reset();

            data_ = aOther.data_;
            capacity_ = aOther.capacity_;
            size_ = aOther.size_;
            space_used_ = aOther.space_used_;
            space_shortage_ = aOther.space_shortage_;

            aOther.data_ = nullptr;
            aOther.capacity_ = 0;
            aOther.size_ = 0;
            aOther.space_used_ = 0;
            aOther.space_shortage_ = 0;
        }
        return *this;
    }

    constexpr payload(char *aStorage, std::size_t aCapacity) noexcept
        : data_{aStorage}, capacity_{aCapacity}
    {
        assert(aStorage != nullptr);
        assert(aCapacity > 0);
    }

    template <size_t N>
    constexpr explicit payload(char (&aStorage)[N]) noexcept
        : payload(aStorage, N)
    {
    }

    constexpr std::size_t size() const noexcept { return size_; }
    constexpr std::size_t space_used() const noexcept { return space_used_; }
    constexpr std::size_t space_shortage() const noexcept
    {
        return space_shortage_;
    }
    constexpr std::size_t capacity() const noexcept { return capacity_; }
    constexpr char const *data() const noexcept { return data_; }

    template <typename T, typename S = utils::remove_cvref_t<T>>
    struct is_nothrow_storable
        : std::conjunction<
              std::is_nothrow_destructible<S>,
              std::conditional_t<
                  is_seq_v<S>, details::is_nothrow_copyable_seq<S>,
                  std::conditional_t<
                      std::is_pointer_v<T>, std::true_type,
                      std::conditional_t<
                          std::is_lvalue_reference_v<T>,
                          details::is_nothrow_copyable<S>,
                          std::disjunction<
                              std::is_nothrow_move_constructible<S>,
                              std::is_nothrow_copy_constructible<S>,
                              std::conjunction<
                                  std::is_nothrow_default_constructible<S>,
                                  std::disjunction<
                                      std::is_nothrow_move_assignable<S>,
                                      std::is_nothrow_copy_assignable<S>>>>>>>>
    {
    };

    template <typename T>
    static constexpr bool is_nothrow_storable_v = is_nothrow_storable<T>::value;

    template <typename T>
    std::enable_if_t<is_nothrow_storable_v<T>> load(T &&aArg) noexcept
    {
        using stored_info = details::stored_info<details::stored_t<T>, char>;

        if (!space_shortage_)
        {
            char *ptr = data_ + space_used_;
            stored_info data_inf(ptr, aArg);
            const auto mem_size = data_inf.size();
            if (space_used_ + mem_size <= capacity_)
            {
                data_inf.write(std::forward<T>(aArg));
                space_used_ += mem_size;
                ++size_;
            }
            else
            {
                space_shortage_ = space_used_ + mem_size - capacity_;
            }
        }
        else
        {
            char *ptr = data_ + capacity_ + space_shortage_;
            space_shortage_ += stored_info(ptr, aArg).size();
        }
    }

    template <typename... Ts>
    bool match() const noexcept
    {
        return details::match<const_info, Ts...>(data_, size_);
    }

    template <typename... Ts>
    bool match() noexcept
    {
        return details::match<info, Ts...>(data_, size_);
    }

    template <typename... Callbacks>
    bool process(Callbacks &&...aCallbacks) const noexcept
    {
        return details::process<const_info>(
            data_, size_, std::forward<Callbacks>(aCallbacks)...);
    }

    template <typename... Callbacks>
    bool process(Callbacks &&...aCallbacks) noexcept
    {
        return details::process<info>(data_, size_,
                                      std::forward<Callbacks>(aCallbacks)...);
    }

    void reset() noexcept
    {
        char *data_ptr = data_;
        while (size_ > 0)
        {
            details::destroy_next(data_ptr);
            --size_;
        }
        space_used_ = 0;
        space_shortage_ = 0;
    }

    void clear() noexcept
    {
        const auto nbytes = space_used_;
        reset();
        std::fill_n(data_, nbytes, 0_u8);
    }

   private:
    char *data_{nullptr};
    std::size_t capacity_{};
    std::size_t size_{};
    std::size_t space_used_{};
    std::size_t space_shortage_{};
};

template <typename T>
struct is_payload : std::false_type
{
};

template <>
struct is_payload<payload> : std::true_type
{
};

template <typename T>
inline constexpr bool is_payload_v = is_payload<T>::value;
}  // namespace cargo
#endif /* cargo_h */
