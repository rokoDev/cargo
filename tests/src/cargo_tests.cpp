#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <cstring>
#include <memory>

#include "cargo/cargo.h"

namespace
{
using ::testing::InSequence;
using ::testing::Return;

using namespace cargo;

template <std::size_t MaxSpace>
class Payload : public ::testing::Test
{
   protected:
    static constexpr std::size_t kMaxSpace = MaxSpace;

    char raw_data_[kMaxSpace]{};
    payload data{raw_data_};

    static bool memvcmp(const void *memptr, unsigned char val,
                        const std::size_t size) noexcept
    {
        if ((0 == size) || (nullptr == memptr))
        {
            return false;
        }
        const unsigned char *mm = static_cast<const unsigned char *>(memptr);
        return (*mm == val) && (memcmp(mm, mm + 1, size - 1) == 0);
    }

    void TearDown() override { data.reset(); }
};

template <std::size_t MaxSpace>
class NonEmptyPayload : public Payload<MaxSpace>
{
   protected:
    NonEmptyPayload()
    {
        Payload<MaxSpace>::data.load(5);
        Payload<MaxSpace>::data.load(10.f);
        Payload<MaxSpace>::data.load(true);
    }
};

class MockSpecialOps
{
   public:
    MOCK_METHOD(void, default_constructor, ());
    MOCK_METHOD(void, copy_constructor, ());
    MOCK_METHOD(void, move_constructor, ());
    MOCK_METHOD(void, copy_assignment, ());
    MOCK_METHOD(void, move_assignment, ());
    MOCK_METHOD(void, destructor, ());
};

class SharedSpecialOps
{
   private:
    static auto getValue() noexcept
    {
        auto p = value_.lock();
        assert(p);
        return p;
    }

   public:
    static void set(std::shared_ptr<MockSpecialOps> aValue) noexcept
    {
        value_ = aValue;
    }

   protected:
    static void destructor() noexcept { getValue()->destructor(); }

    static void default_constructor() noexcept
    {
        getValue()->default_constructor();
    }

    static void copy_constructor() noexcept { getValue()->copy_constructor(); }

    static void move_constructor() noexcept { getValue()->move_constructor(); }

    static void copy_assignment() noexcept { getValue()->copy_assignment(); }

    static void move_assignment() noexcept { getValue()->move_assignment(); }

   private:
    static std::weak_ptr<MockSpecialOps> value_;
};

std::weak_ptr<MockSpecialOps> SharedSpecialOps::value_{};

template <std::size_t MaxSpace>
class PayloadWithFixture : public Payload<MaxSpace>
{
   protected:
    void SetUp() override
    {
        value_ = std::make_shared<MockSpecialOps>();
        SharedSpecialOps::set(value_);
    }

    void TearDown() override
    {
        Payload<MaxSpace>::TearDown();
        value_ = nullptr;
    }

    std::shared_ptr<MockSpecialOps> value_;
};

using FixturePayload64 = PayloadWithFixture<64>;
using FixturePayload128 = PayloadWithFixture<128>;

struct Ctor
{
};

struct Dector
{
};

struct CopyCtor
{
};

struct MoveCtor
{
};

struct CopyAssign
{
};

struct MoveAssign
{
};

template <typename... NoThrowOps>
class Element : public SharedSpecialOps
{
    template <typename T>
    static constexpr bool is_noexcept =
        utils::type_list<NoThrowOps...>::template contains_v<T>;

   public:
    ~Element() noexcept(is_noexcept<Dector>) { destructor(); }

    Element() noexcept(is_noexcept<Ctor>) { default_constructor(); }

    Element(const Element &) noexcept(is_noexcept<CopyCtor>)
    {
        copy_constructor();
    }

    Element(Element &&) noexcept(is_noexcept<MoveCtor>) { move_constructor(); }

    Element &operator=(const Element &) noexcept(is_noexcept<CopyAssign>)
    {
        copy_assignment();
        return *this;
    }

    Element &operator=(Element &&) noexcept(is_noexcept<MoveAssign>)
    {
        move_assignment();
        return *this;
    }

    friend constexpr bool operator==(const Element &, const Element &) noexcept
    {
        return true;
    }
};
using NoThrow =
    Element<Dector, Ctor, CopyCtor, MoveCtor, CopyAssign, MoveAssign>;
using NoThrowCopyCtor = Element<Dector, CopyCtor>;
using NoThrowMoveCtor = Element<Dector, MoveCtor>;
using NoThrowCtorAndCopyAssign = Element<Dector, Ctor, CopyAssign>;
using NoThrowCtorAndMoveAssign = Element<Dector, Ctor, MoveAssign>;
using ThrowingT = Element<>;

using Payload64 = Payload<64>;
using Payload128 = Payload<128>;

using PayloadWithData = NonEmptyPayload<64>;
using Payload63 = Payload<63>;
}  // namespace

TEST(PTest, ConstructWithDataAndSize)
{
    constexpr std::size_t s = 10;
    char data[s];
    char *d = data;
    const payload p(d, s);

    ASSERT_EQ(p.capacity(), s);
    ASSERT_EQ(p.data(), data);
}

TEST(PTest, ConstructWithCArray)
{
    constexpr std::size_t s = 10;
    char data[s];
    const payload p(data);

    ASSERT_EQ(p.capacity(), s);
    ASSERT_EQ(p.data(), data);
}

TEST_F(PayloadWithData, Size) { ASSERT_EQ(data.size(), 3); }

TEST_F(PayloadWithData, Match1)
{
    const bool isMatched = data.match<int, float, bool>();
    ASSERT_TRUE(isMatched);
}

TEST_F(PayloadWithData, Match2)
{
    using types = utils::type_list<int, float, bool>;
    ASSERT_TRUE(data.match<types>());
}

TEST_F(PayloadWithData, Match3)
{
    using types = utils::type_list<int, float>;
    ASSERT_TRUE(data.match<types>());
}

TEST_F(PayloadWithData, Match4)
{
    using types = utils::type_list<int>;
    ASSERT_TRUE(data.match<types>());
}

TEST_F(PayloadWithData, Match5)
{
    using types = utils::type_list<bool>;
    ASSERT_FALSE(data.match<types>());
}

TEST_F(PayloadWithData, Match6)
{
    using types = utils::type_list<bool, float, char, int>;
    ASSERT_FALSE(data.match<types>());
}

TEST_F(PayloadWithData, Match7)
{
    using types = utils::type_list<const int &>;
    ASSERT_TRUE(data.match<types>());
}

TEST_F(PayloadWithData, Match8)
{
    using types = utils::type_list<const int &, const float, bool>;
    ASSERT_TRUE(data.match<types>());
}

TEST_F(PayloadWithData, Match9)
{
    auto it = data.begin();
    ASSERT_TRUE(it->match<int>());
    ASSERT_TRUE(it->match<int &>());
    ASSERT_FALSE(it->match<int &&>());
    ASSERT_TRUE(it->match<const int>());
    ASSERT_TRUE(it->match<const int &>());
    ASSERT_FALSE(it->match<const int &&>());
}

TEST_F(PayloadWithData, Match10)
{
    auto it = data.cbegin();
    ASSERT_TRUE(it->match<int>());
    ASSERT_FALSE(it->match<int &>());
    ASSERT_FALSE(it->match<int &&>());
    ASSERT_TRUE(it->match<const int>());
    ASSERT_TRUE(it->match<const int &>());
    ASSERT_FALSE(it->match<const int &&>());
}

TEST_F(FixturePayload64, Match11)
{
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    ThrowingT value;
    ThrowingT *value_ptr = &value;
    data.load(value_ptr);
    ASSERT_TRUE(data.begin()->match<ThrowingT const *>());
    ASSERT_TRUE(data.begin()->match<ThrowingT *>());
    ASSERT_TRUE(data.cbegin()->match<ThrowingT const *>());
    ASSERT_FALSE(data.cbegin()->match<ThrowingT *>());
}

TEST_F(FixturePayload64, Match12)
{
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    ThrowingT value;
    ThrowingT const *value_ptr = &value;
    data.load(value_ptr);
    ASSERT_TRUE(data.begin()->match<ThrowingT const *>());
    ASSERT_FALSE(data.begin()->match<ThrowingT *>());
    ASSERT_TRUE(data.cbegin()->match<ThrowingT const *>());
    ASSERT_FALSE(data.cbegin()->match<ThrowingT *>());
}

TEST_F(FixturePayload64, Match13)
{
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    ThrowingT value;
    data.load(std::ref(value));
    ASSERT_TRUE(data.begin()->match<std::reference_wrapper<ThrowingT>>());
    ASSERT_TRUE(data.begin()->match<std::reference_wrapper<const ThrowingT>>());
    ASSERT_FALSE(data.cbegin()->match<std::reference_wrapper<ThrowingT>>());
    ASSERT_TRUE(
        data.cbegin()->match<std::reference_wrapper<const ThrowingT>>());
    ASSERT_FALSE(data.begin()->match<ThrowingT &>());
    ASSERT_FALSE(data.cbegin()->match<const ThrowingT &>());
}

TEST_F(FixturePayload64, Match14)
{
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    ThrowingT value;
    data.load(std::cref(value));
    ASSERT_FALSE(data.begin()->match<std::reference_wrapper<ThrowingT>>());
    ASSERT_TRUE(data.begin()->match<std::reference_wrapper<const ThrowingT>>());
    ASSERT_FALSE(data.cbegin()->match<std::reference_wrapper<ThrowingT>>());
    ASSERT_TRUE(
        data.cbegin()->match<std::reference_wrapper<const ThrowingT>>());
    ASSERT_FALSE(data.begin()->match<ThrowingT &>());
    ASSERT_FALSE(data.begin()->match<const ThrowingT &>());
    ASSERT_FALSE(data.cbegin()->match<const ThrowingT &>());
}

TEST_F(FixturePayload64, Match15)
{
    EXPECT_CALL(*value_, default_constructor()).Times(10);
    EXPECT_CALL(*value_, copy_constructor()).Times(10);
    EXPECT_CALL(*value_, destructor()).Times(20);
    using seq_t = seq<NoThrowCopyCtor, std::size_t>;
    using cseq_t = seq<const NoThrowCopyCtor, std::size_t>;

    NoThrowCopyCtor values[10] = {{}, {}, {}, {}, {}, {}, {}, {}, {}, {}};
    data.load(seq_t(values));
    ASSERT_TRUE(data.begin()->match<seq_t>());
    ASSERT_TRUE(data.begin()->match<cseq_t>());
    ASSERT_FALSE(data.cbegin()->match<seq_t>());
    ASSERT_TRUE(data.cbegin()->match<cseq_t>());
}

TEST_F(FixturePayload64, Match16)
{
    EXPECT_CALL(*value_, default_constructor()).Times(20);
    EXPECT_CALL(*value_, copy_assignment()).Times(10);
    EXPECT_CALL(*value_, destructor()).Times(20);
    using seq_t = seq<NoThrowCtorAndCopyAssign, std::size_t>;
    using cseq_t = seq<const NoThrowCtorAndCopyAssign, std::size_t>;

    NoThrowCtorAndCopyAssign values[10] = {{}, {}, {}, {}, {},
                                           {}, {}, {}, {}, {}};
    data.load(seq_t(values));
    ASSERT_TRUE(data.begin()->match<seq_t>());
    ASSERT_TRUE(data.begin()->match<cseq_t>());
    ASSERT_FALSE(data.cbegin()->match<seq_t>());
    ASSERT_TRUE(data.cbegin()->match<cseq_t>());
}

TEST_F(FixturePayload64, Match17)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    data.load(NoThrowCopyCtor());

    auto it = data.begin();
    ASSERT_TRUE(it->match<NoThrowCopyCtor>());
    ASSERT_TRUE(it->match<const NoThrowCopyCtor>());

    auto cit = data.cbegin();
    ASSERT_TRUE(cit->match<NoThrowCopyCtor>());
    ASSERT_TRUE(cit->match<const NoThrowCopyCtor>());
}

TEST_F(FixturePayload64, Match18)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);

    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);

    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);

    EXPECT_CALL(*value_, destructor()).Times(1);

    std::size_t processed_index{};
    const auto value_handler = std::make_tuple(
        [&processed_index](char) noexcept { processed_index = 1; },
        [&processed_index](NoThrowCopyCtor const *) noexcept
        { processed_index = 2; },
        [&processed_index](NoThrowCopyCtor) noexcept { processed_index = 3; },
        [&processed_index](float &) noexcept { processed_index = 4; });

    const auto const_value_handler = std::make_tuple(
        [&processed_index](char) noexcept { processed_index = 5; },
        [&processed_index](const NoThrowCopyCtor) noexcept
        { processed_index = 6; },
        [&processed_index](std::reference_wrapper<const int>) noexcept
        { processed_index = 7; });

    data.load(NoThrowCopyCtor());

    auto it = data.begin();
    ASSERT_TRUE(it->match<NoThrowCopyCtor>());
    ASSERT_TRUE(it->match<const NoThrowCopyCtor>());

    auto cit = data.cbegin();
    ASSERT_TRUE(cit->match<NoThrowCopyCtor>());
    ASSERT_TRUE(cit->match<const NoThrowCopyCtor>());

    ASSERT_TRUE(data.process(value_handler));
    ASSERT_EQ(processed_index, 3);

    ASSERT_TRUE(data.process(const_value_handler));
    ASSERT_EQ(processed_index, 6);
}

TEST_F(FixturePayload64, Match19)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, copy_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    std::size_t processed_index{};
    const auto value_handler = std::make_tuple(
        [&processed_index](char) noexcept { processed_index = 1; },
        [&processed_index](NoThrowCtorAndCopyAssign const *) noexcept
        { processed_index = 2; },
        [&processed_index](NoThrowCtorAndCopyAssign) noexcept
        { processed_index = 3; },
        [&processed_index](float &) noexcept { processed_index = 4; });

    const auto const_value_handler = std::make_tuple(
        [&processed_index](char) noexcept { processed_index = 5; },
        [&processed_index](const NoThrowCtorAndCopyAssign) noexcept
        { processed_index = 6; },
        [&processed_index](std::reference_wrapper<const int>) noexcept
        { processed_index = 7; },
        [&processed_index](const NoThrowCtorAndCopyAssign &) noexcept
        { processed_index = 8; });

    data.load(NoThrowCtorAndCopyAssign());

    auto it = data.begin();
    ASSERT_FALSE(it->match<NoThrowCtorAndCopyAssign>());
    ASSERT_FALSE(it->match<const NoThrowCtorAndCopyAssign>());
    ASSERT_TRUE(it->match<NoThrowCtorAndCopyAssign &>());
    ASSERT_TRUE(it->match<const NoThrowCtorAndCopyAssign &>());

    auto cit = data.cbegin();
    ASSERT_FALSE(cit->match<NoThrowCtorAndCopyAssign>());
    ASSERT_FALSE(cit->match<const NoThrowCtorAndCopyAssign>());
    ASSERT_FALSE(cit->match<NoThrowCtorAndCopyAssign &>());
    ASSERT_TRUE(cit->match<const NoThrowCtorAndCopyAssign &>());

    ASSERT_FALSE(data.process(value_handler));
    ASSERT_EQ(processed_index, 0);

    ASSERT_TRUE(data.process(const_value_handler));
    ASSERT_EQ(processed_index, 8);
}

TEST_F(FixturePayload64, LoadEmptySeq)
{
    using seq_t = seq<NoThrowCopyCtor, std::size_t>;
    using cseq_t = seq<const NoThrowCopyCtor, std::size_t>;
    constexpr std::size_t kCount = 3;

    InSequence call_seq;
    EXPECT_CALL(*value_, default_constructor()).Times(kCount);
    EXPECT_CALL(*value_, destructor()).Times(kCount);

    NoThrowCopyCtor values[kCount] = {{}, {}, {}};
    data.load(seq_t(values, 0));

    auto it = data.begin();
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_TRUE(it->match<seq_t>());
    ASSERT_TRUE(it->match<cseq_t>());
    ASSERT_EQ(it->get<seq_t>().size(), 0);
    ASSERT_EQ(it->get<seq_t>().data(), nullptr);
    ASSERT_EQ(it->get<cseq_t>().size(), 0);
    ASSERT_EQ(it->get<cseq_t>().data(), nullptr);
    ASSERT_FALSE(it->match<seq_t &>());
    ASSERT_FALSE(it->match<const seq_t &>());
    ASSERT_FALSE(it->match<cseq_t &>());
    ASSERT_FALSE(it->match<const cseq_t &>());

    auto cit = data.cbegin();
    ASSERT_EQ(&cit->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_FALSE(cit->match<seq_t>());
    ASSERT_TRUE(cit->match<cseq_t>());
    ASSERT_EQ(cit->get<cseq_t>().size(), 0);
    ASSERT_EQ(cit->get<cseq_t>().data(), nullptr);
    ASSERT_FALSE(cit->match<seq_t &>());
    ASSERT_FALSE(cit->match<const seq_t &>());
    ASSERT_FALSE(cit->match<cseq_t &>());
    ASSERT_FALSE(cit->match<const cseq_t &>());
}

TEST_F(Payload64, LoadSeq)
{
    using seq_t = seq<int, std::size_t>;
    using cseq_t = seq<const int, std::size_t>;

    int values[10] = {{}, {}, {}, {}, {}, {}, {}, {}, {}, {}};
    data.load(seq_t(values));

    ASSERT_EQ(&data.begin()->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_EQ(&data.cbegin()->type_name.get(), &type_name::kName<seq_t>);
    static_assert(std::is_same_v<decltype(data.begin()->get<seq_t>()), seq_t>);
    static_assert(
        std::is_same_v<decltype(data.cbegin()->get<cseq_t>()), cseq_t>);
}

TEST_F(Payload64, LoadConstSeq)
{
    using seq_t = seq<int, std::size_t>;
    using cseq_t = seq<const int, std::size_t>;

    int values[10] = {{}, {}, {}, {}, {}, {}, {}, {}, {}, {}};
    data.load(cseq_t(values));

    ASSERT_EQ(&data.begin()->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_EQ(&data.cbegin()->type_name.get(), &type_name::kName<seq_t>);
}

TEST_F(FixturePayload128, Get1)
{
    using seq_t = seq<NoThrowCopyCtor, std::uint8_t>;
    using cseq_t = seq<const NoThrowCopyCtor, std::uint8_t>;
    static_assert(
        std::is_nothrow_invocable_r_v<seq_t, decltype(&info::get<seq_t>),
                                      info>);
    static_assert(std::is_nothrow_invocable_r_v<
                  cseq_t, decltype(&const_info::get<cseq_t>), const_info>);

    InSequence call_seq;
    EXPECT_CALL(*value_, default_constructor()).Times(5);
    EXPECT_CALL(*value_, copy_constructor()).Times(5);
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, copy_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(11);

    NoThrowCopyCtor values[5] = {{}, {}, {}, {}, {}};
    data.load(seq_t(values));

    using float_seq_t = seq<float, std::uint8_t>;
    using cfloat_seq_t = seq<const float, std::uint8_t>;
    float fvalues[3] = {1.2f, 2.3f, 4.5f};
    data.load(cfloat_seq_t(fvalues));

    data.load(NoThrowCtorAndCopyAssign());

    using char_seq_t = seq<char, std::uint8_t>;
    using cchar_seq_t = seq<const char, std::uint8_t>;
    char cvalues[4] = {'w', 'o', 'r', 'd'};
    data.load(cchar_seq_t(cvalues));

    auto it = data.begin();
    auto cit = data.cbegin();
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_EQ(&cit->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_EQ(it->get<seq_t>(), seq_t(values));
    ASSERT_EQ(cit->get<cseq_t>(), cseq_t(values));

    ++it;
    ++cit;
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<float_seq_t>);
    ASSERT_EQ(&cit->type_name.get(), &type_name::kName<float_seq_t>);
    ASSERT_EQ(it->get<float_seq_t>(), float_seq_t(fvalues));
    ASSERT_EQ(cit->get<cfloat_seq_t>(), cfloat_seq_t(fvalues));

    ++it;
    ++cit;
    ASSERT_EQ(&it->type_name.get(),
              &type_name::kName<NoThrowCtorAndCopyAssign>);
    ASSERT_EQ(&cit->type_name.get(),
              &type_name::kName<NoThrowCtorAndCopyAssign>);
    ASSERT_EQ(it->get<NoThrowCtorAndCopyAssign &>(),
              NoThrowCtorAndCopyAssign());
    ASSERT_EQ(cit->get<const NoThrowCtorAndCopyAssign &>(),
              NoThrowCtorAndCopyAssign());

    ++it;
    ++cit;
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<char_seq_t>);
    ASSERT_EQ(&cit->type_name.get(), &type_name::kName<char_seq_t>);
    ASSERT_EQ(it->get<char_seq_t>(), char_seq_t(cvalues));
    ASSERT_EQ(it->get<cchar_seq_t>(), cchar_seq_t(cvalues));
    ASSERT_EQ(cit->get<cchar_seq_t>(), cchar_seq_t(cvalues));
}

TEST_F(Payload64, IterateEmpty)
{
    std::size_t count{};
    for (auto it = data.cbegin(); it != data.cend(); ++it)
    {
        ++count;
    }

    ASSERT_EQ(count, data.size());
    ASSERT_EQ(data.begin(), data.end());
    ASSERT_EQ(data.cbegin(), data.cend());

    for ([[maybe_unused]] const auto &value_info: data)
    {
        ++count;
    }

    ASSERT_EQ(count, data.size());
}

TEST_F(Payload64, IterateOneViaForConst)
{
    data.load(5);

    using CValuePtrT = char const *;
    static_assert(std::is_same_v<decltype(data.cbegin()->value), CValuePtrT>);

    using ConstInfoT = payload::const_iterator::value_type &;
    static_assert(std::is_same_v<decltype(*data.cbegin()), ConstInfoT>);

    std::size_t count{};
    for (auto it = data.cbegin(); it != data.cend(); ++it)
    {
        ++count;
    }

    ASSERT_EQ(count, data.size());
}

TEST_F(Payload64, IterateOneViaForConstPostInc)
{
    data.load(true);
    data.load(5);

    using CValuePtrT = char const *;
    static_assert(std::is_same_v<decltype(data.cbegin()->value), CValuePtrT>);

    using ConstInfoT = payload::const_iterator::value_type &;
    static_assert(std::is_same_v<decltype(*data.cbegin()), ConstInfoT>);

    std::size_t count{};
    for (auto it = data.cbegin(); it != data.cend(); it++)
    {
        ++count;
    }

    ASSERT_EQ(count, data.size());
}

TEST_F(Payload64, IterateOneViaForNonConst)
{
    data.load(5);

    using ValuePtrT = char *;
    static_assert(std::is_same_v<decltype(data.begin()->value), ValuePtrT>);

    using InfoT = payload::iterator::value_type &;
    static_assert(std::is_same_v<decltype(*data.begin()), InfoT>);

    std::size_t count{};
    for (auto it = data.begin(); it != data.end(); ++it)
    {
        ++count;
    }

    ASSERT_EQ(count, data.size());
}

TEST_F(Payload64, IterateOneViaRangeFor)
{
    data.load(5);

    std::size_t count{};
    using InfoT = const payload::iterator::value_type &;
    for (const auto &value_info: data)
    {
        static_assert(std::is_same_v<decltype(value_info), InfoT>);
        ++count;
    }

    ASSERT_EQ(count, data.size());
}

TEST_F(Payload64, IterateViaFor)
{
    data.load(5);
    data.load(10.f);
    data.load(true);

    using ValuePtrT = char *;
    static_assert(std::is_same_v<decltype(data.begin()->value), ValuePtrT>);

    constexpr auto type_names =
        utils::make_array(&type_name::kName<int>, &type_name::kName<float>,
                          &type_name::kName<bool>);
    auto nameIt = type_names.cbegin();
    for (auto it = data.cbegin(); it != data.cend(); ++it, ++nameIt)
    {
        ASSERT_EQ(&it->type_name.get(), *nameIt);
    }

    ASSERT_EQ(nameIt, type_names.cend());
}

TEST_F(Payload64, IterateViaRangeFor)
{
    data.load(5);
    data.load(10.f);
    data.load(true);

    constexpr auto type_names =
        utils::make_array(&type_name::kName<int>, &type_name::kName<float>,
                          &type_name::kName<bool>);

    std::size_t count{};
    for (const auto &value_info: data)
    {
        ASSERT_EQ(&value_info.type_name.get(), type_names[count]);
        ++count;
    }

    ASSERT_EQ(count, data.size());
}

TEST_F(Payload64, Load)
{
    data.load(5);
    data.load(10_u64);
    ASSERT_EQ(data.size(), 2);
}

TEST_F(PayloadWithData, Reset)
{
    data.reset();
    ASSERT_EQ(data.size(), 0);
    ASSERT_FALSE(memvcmp(raw_data_, 0, kMaxSpace));
}

TEST_F(PayloadWithData, Clear)
{
    data.clear();
    ASSERT_EQ(data.size(), 0);
    ASSERT_TRUE(memvcmp(raw_data_, 0, kMaxSpace));
}

TEST_F(Payload63, ForbidLoadAfterShortage)
{
    data.load(true);
    while (!data.space_shortage())
    {
        data.load(5_i64);
    }
    const auto shortage = data.space_shortage();
    ASSERT_TRUE(shortage > 0);

    data.load(true);
    const auto next_shortage = data.space_shortage();
    ASSERT_TRUE(shortage < next_shortage);

    data.load(3.f);
    const auto next_next_shortage = data.space_shortage();
    ASSERT_TRUE(next_shortage < next_next_shortage);
}

TEST_F(Payload63, RestoreAbilityToLoadAfterClear)
{
    data.load(true);
    while (!data.space_shortage())
    {
        data.load(5_i64);
    }
    const auto shortage = data.space_shortage();
    ASSERT_TRUE(shortage > 0);

    data.clear();
    ASSERT_TRUE(data.space_shortage() == 0);

    data.load(false);
    data.load(10_i64);

    ASSERT_TRUE(data.space_shortage() == 0);
    ASSERT_TRUE(data.size() == 2);
}

TEST_F(Payload63, RestoreAbilityToLoadAfterReset)
{
    data.load(true);
    while (!data.space_shortage())
    {
        data.load(5_i64);
    }
    const auto shortage = data.space_shortage();
    ASSERT_TRUE(shortage > 0);

    data.reset();
    ASSERT_TRUE(data.space_shortage() == 0);

    data.load(false);
    data.load(10_i64);

    ASSERT_TRUE(data.space_shortage() == 0);
    ASSERT_TRUE(data.size() == 2);
}

TEST_F(Payload64, LoadPointer)
{
    int v = 5;
    int *p = &v;
    data.load(p);
    ASSERT_EQ(&data.begin()->type_name.get(), &type_name::kName<int *>);
}

TEST_F(Payload64, LoadPointerToConst)
{
    int v = 5;
    int const *p = &v;
    data.load(p);
    ASSERT_EQ(&data.begin()->type_name.get(), &type_name::kName<int const *>);
}

TEST_F(Payload64, LoadConstPointerToConst)
{
    int v = 5;
    int const *const p = &v;
    data.load(p);
    ASSERT_EQ(&data.begin()->type_name.get(), &type_name::kName<int const *>);
}

TEST_F(Payload64, LoadCStr)
{
    using cchar_seq_t = seq<const char, std::uint8_t>;
    using char_seq_t = seq<char, std::uint8_t>;
    using int_seq_t = seq<int, std::uint8_t>;
    char const *str = "Hello";
    const auto len = static_cast<cchar_seq_t::size_type>(std::strlen(str) + 1);
    cchar_seq_t s(str, len);
    data.load(cchar_seq_t(str));
    data.load(cchar_seq_t("World!"));

    ASSERT_EQ(data.size(), 2);

    auto it = data.begin();
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<char_seq_t>);
    ASSERT_EQ(it->get<cchar_seq_t>(), s);

    ++it;
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<char_seq_t>);
    ASSERT_EQ(it->get<cchar_seq_t>(), cchar_seq_t("World!"));

    int ar[] = {1234, -5678};
    data.load(int_seq_t(ar));
    ++it;
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<int_seq_t>);
    int_seq_t restored(it->get<int_seq_t>());
    ASSERT_NE(restored.data(), ar);
    ASSERT_EQ(restored, int_seq_t(ar));
}

TEST_F(Payload64, LoadEmptyCStr)
{
    using data_t = char;
    using seq_t = seq<data_t, std::size_t>;
    using cseq_t = seq<const data_t, std::size_t>;

    char const *str = "";
    data.load(cseq_t(str));

    auto it = data.begin();
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_TRUE(it->match<seq_t>());
    ASSERT_TRUE(it->match<cseq_t>());
    ASSERT_EQ(it->get<seq_t>().size(), 1);
    ASSERT_NE(it->get<seq_t>().data(), nullptr);
    ASSERT_EQ(*it->get<seq_t>().data(), '\0');
    ASSERT_EQ(it->get<cseq_t>().size(), 1);
    ASSERT_NE(it->get<cseq_t>().data(), nullptr);
    ASSERT_EQ(*it->get<cseq_t>().data(), '\0');

    auto cit = data.cbegin();
    ASSERT_EQ(&cit->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_FALSE(cit->match<seq_t>());
    ASSERT_TRUE(cit->match<cseq_t>());
    ASSERT_EQ(cit->get<cseq_t>().size(), 1);
    ASSERT_NE(cit->get<cseq_t>().data(), nullptr);
    ASSERT_EQ(*cit->get<cseq_t>().data(), '\0');
}

TEST_F(Payload64, LoadNullCStr)
{
    using data_t = char;
    using seq_t = seq<data_t, std::size_t>;
    using cseq_t = seq<const data_t, std::size_t>;

    char *str = nullptr;
    data.load(seq_t(str));

    auto it = data.begin();
    ASSERT_EQ(&it->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_TRUE(it->match<seq_t>());
    ASSERT_TRUE(it->match<cseq_t>());
    ASSERT_EQ(it->get<seq_t>().size(), 0);
    ASSERT_EQ(it->get<seq_t>().data(), nullptr);
    ASSERT_EQ(it->get<cseq_t>().size(), 0);
    ASSERT_EQ(it->get<cseq_t>().data(), nullptr);

    auto cit = data.cbegin();
    ASSERT_EQ(&cit->type_name.get(), &type_name::kName<seq_t>);
    ASSERT_FALSE(cit->match<seq_t>());
    ASSERT_TRUE(cit->match<cseq_t>());
    ASSERT_EQ(cit->get<cseq_t>().size(), 0);
    ASSERT_EQ(cit->get<cseq_t>().data(), nullptr);
}

TEST_F(Payload64, MoveConstructor)
{
    int v = 5;
    int *p = &v;
    data.load(p);

    using cchar_seq_t = seq<const char, std::uint8_t>;
    char const *str = "Hello";
    data.load(cchar_seq_t(str));

    payload moved(std::move(data));

    ASSERT_EQ(data.data(), nullptr);
    ASSERT_EQ(data.capacity(), 0);
    ASSERT_EQ(data.size(), 0);
    ASSERT_EQ(data.space_used(), 0);
    ASSERT_EQ(data.space_shortage(), 0);

    ASSERT_TRUE((moved.match<int *, cchar_seq_t>()));

    auto it = moved.begin();
    ASSERT_EQ(it->get<int *>(), p);

    ++it;
    ASSERT_EQ(it->get<cchar_seq_t>(), cchar_seq_t("Hello"));
}

TEST_F(FixturePayload64, MoveAssignment)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, move_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    int v = 5;
    int *p = &v;
    data.load(p);

    using cchar_seq_t = cargo::seq<const char, std::uint8_t>;
    char const *str = "Hello";
    data.load(cchar_seq_t(str));

    char raw_data[32]{};
    payload move_assigned(raw_data);
    move_assigned.load(NoThrow{});

    move_assigned = std::move(data);

    ASSERT_EQ(data.data(), nullptr);
    ASSERT_EQ(data.capacity(), 0);
    ASSERT_EQ(data.size(), 0);
    ASSERT_EQ(data.space_used(), 0);
    ASSERT_EQ(data.space_shortage(), 0);

    ASSERT_TRUE((move_assigned.match<int *, cchar_seq_t>()));

    auto it = move_assigned.begin();
    ASSERT_EQ(it->get<int *>(), p);

    ++it;
    ASSERT_EQ(it->get<cchar_seq_t>(), cchar_seq_t("Hello"));
}

TEST_F(FixturePayload64, AddNoThrowCopyCtorAsLValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        NoThrowCopyCtor obj;
        payload pl(raw_data_);
        pl.load(obj);
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCopyCtor>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCopyCtorAsRValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        NoThrowCopyCtor obj;
        pl.load(std::move(obj));
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCopyCtor>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCopyCtorAsValue)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        pl.load(NoThrowCopyCtor());
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCopyCtor>);
    }
}

TEST_F(FixturePayload64, AddConstNoThrowCopyCtorAsLValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, copy_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        const NoThrowCopyCtor obj;
        payload pl(raw_data_);
        pl.load(obj);
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCopyCtor>);
    }
}

TEST(Payload, IsNothrowStorable)
{
    static_assert(not payload::is_nothrow_storable_v<NoThrowMoveCtor &>);
    static_assert(
        not payload::is_nothrow_storable_v<NoThrowCtorAndMoveAssign &>);
    static_assert(payload::is_nothrow_storable_v<int>);
    static_assert(payload::is_nothrow_storable_v<int &>);
    static_assert(payload::is_nothrow_storable_v<int const *>);
    static_assert(payload::is_nothrow_storable_v<int *>);
    static_assert(payload::is_nothrow_storable_v<seq<int, std::size_t>>);
    static_assert(payload::is_nothrow_storable_v<seq<const int, std::size_t>>);
    static_assert(
        not payload::is_nothrow_storable_v<seq<NoThrowMoveCtor, std::size_t>>);
    static_assert(not payload::is_nothrow_storable_v<
                  seq<const NoThrowMoveCtor, std::size_t>>);
    static_assert(
        not payload::is_nothrow_storable_v<seq<ThrowingT, std::size_t>>);
    static_assert(not payload::is_nothrow_storable_v<
                  seq<NoThrowCtorAndMoveAssign, std::size_t>>);
    static_assert(payload::is_nothrow_storable_v<
                  seq<NoThrowCtorAndCopyAssign, std::size_t>>);
    static_assert(
        payload::is_nothrow_storable_v<seq<NoThrowCopyCtor, std::size_t>>);
    static_assert(payload::is_nothrow_storable_v<
                  seq<const NoThrowCopyCtor, std::size_t>>);
}

TEST_F(FixturePayload64, AddNoThrowMoveCtorAsRValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, move_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        NoThrowMoveCtor obj;
        pl.load(std::move(obj));
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowMoveCtor>);
    }
}

TEST_F(FixturePayload64, AddNoThrowMoveCtorAsValue)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, move_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        pl.load(NoThrowMoveCtor());
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowMoveCtor>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCtorAndCopyAssignAsLValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, copy_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        NoThrowCtorAndCopyAssign obj;
        pl.load(obj);
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCtorAndCopyAssign>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCtorAndCopyAssignAsRValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, copy_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        NoThrowCtorAndCopyAssign obj;
        pl.load(std::move(obj));
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCtorAndCopyAssign>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCtorAndCopyAssignAsValue)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, copy_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        pl.load(NoThrowCtorAndCopyAssign());
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCtorAndCopyAssign>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCtorAndMoveAssignAsRValueRef)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, move_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        NoThrowCtorAndMoveAssign obj;
        pl.load(std::move(obj));
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCtorAndMoveAssign>);
    }
}

TEST_F(FixturePayload64, AddNoThrowCtorAndMoveAssignAsValue)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(2);
    EXPECT_CALL(*value_, move_assignment()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(2);

    {
        payload pl(raw_data_);
        pl.load(NoThrowCtorAndMoveAssign());
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<NoThrowCtorAndMoveAssign>);
    }
}

TEST_F(FixturePayload64, AddRefToThrowingT)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);

    {
        payload pl(raw_data_);
        ThrowingT obj;
        auto ref = std::ref(obj);
        pl.load(ref);
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<std::reference_wrapper<ThrowingT>>);
    }
}

TEST_F(FixturePayload64, AddConstRefToThrowingT)
{
    InSequence seq;
    EXPECT_CALL(*value_, default_constructor()).Times(1);
    EXPECT_CALL(*value_, destructor()).Times(1);

    {
        payload pl(raw_data_);
        ThrowingT obj;
        pl.load(std::cref(obj));
        ASSERT_EQ(&pl.begin()->type_name.get(),
                  &type_name::kName<std::reference_wrapper<ThrowingT const>>);
    }
}

TEST_F(Payload128, ProcessTuple)
{
    using char_seq = seq<char, std::uint8_t>;
    using const_char_seq = seq<const char, std::uint8_t>;

    data.load(5);

    data.load(1.23f);

    char const *null_cstr = nullptr;
    data.load(const_char_seq(null_cstr));

    char const *empty_cstr = "";
    data.load(const_char_seq(empty_cstr));

    auto cstr = "word";
    data.load(const_char_seq(cstr));

    double d = 5.4321;
    data.load(std::cref(d));

    std::size_t processed_index{};
    const auto handlers = std::make_tuple(
        [&processed_index](char) noexcept { processed_index = 1; },
        [&processed_index](float, char) noexcept { processed_index = 2; },
        [&processed_index](float, char, char *) noexcept
        { processed_index = 3; },
        [&processed_index](int, float &, char_seq, char_seq, char_seq,
                           std::reference_wrapper<const double>) noexcept
        { processed_index = 4; },
        [&processed_index](int, const float &, const_char_seq, const_char_seq,
                           const_char_seq,
                           std::reference_wrapper<const double>) noexcept
        { processed_index = 5; });
    ASSERT_TRUE(data.process(handlers));
    ASSERT_EQ(processed_index, 4);

    const auto &const_data = data;
    ASSERT_TRUE(const_data.process(handlers));
    ASSERT_EQ(processed_index, 5);
}
