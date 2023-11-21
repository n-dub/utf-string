#pragma once
#include <cassert>
#include <codecvt>
#include <locale>
#include <ostream>
#include <string>
#include <type_traits>
#include <utfimpl/Unicode.hpp>
#include <vector>

namespace utfstring
{
    enum class ParseErrorCode : uint32_t
    {
        None,
        Overflow,
        InvalidSyntax,
        UnexpectedEnd
    };

    class ParseError final
    {
        // In case of 32-bit data
        // | 4-bit error code | 28-bit position |
        uint32_t m_Data;

        inline static constexpr uint32_t PositionBitCount = 28;
        inline static constexpr uint32_t PositionMask     = (1 << PositionBitCount) - 1;

    public:
        inline ParseError() noexcept
            : ParseError(ParseErrorCode::None, 0)
        {
        }

        inline ParseError(ParseErrorCode code) noexcept
            : ParseError(code, 0)
        {
        }

        inline ParseError(ParseErrorCode code, size_t position) noexcept
            : m_Data(static_cast<uint32_t>(position) | (static_cast<uint32_t>(code) << PositionBitCount))
        {
        }

        [[nodiscard]] inline ParseErrorCode GetCode() const noexcept
        {
            return static_cast<ParseErrorCode>(m_Data >> PositionBitCount);
        }

        [[nodiscard]] inline uint32_t GetPosition() const noexcept
        {
            return m_Data & PositionMask;
        }

        inline friend bool operator==(ParseError parseError, ParseErrorCode code) noexcept
        {
            return parseError.GetCode() == code;
        }

        inline friend bool operator!=(ParseError parseError, ParseErrorCode code) noexcept
        {
            return parseError.GetCode() != code;
        }

        inline friend bool operator==(ParseError lhs, ParseError rhs) noexcept
        {
            return lhs.GetCode() == rhs.GetCode() && lhs.GetPosition() == rhs.GetPosition();
        }

        inline friend bool operator!=(ParseError lhs, ParseError rhs) noexcept
        {
            return lhs.GetCode() != rhs.GetCode() && lhs.GetPosition() != rhs.GetPosition();
        }
    };

    template<class T>
    struct ValueParser : std::false_type
    {
    };

    //! \brief A slice of String.
    class StringSlice final
    {
        const TChar* m_Data;
        size_t m_Size;

        ParseError TryToIntImpl(int64_t& result) const;
        ParseError TryToUIntImpl(uint64_t& result) const;

        ParseError TryToFloatImpl(double& result) const;

        template<class T>
        inline static constexpr bool is_signed_integer_v =
            std::is_signed_v<T> && std::is_integral_v<T> && !std::is_same_v<T, bool>;

        template<class T>
        inline static constexpr bool is_unsigned_integer_v =
            std::is_unsigned_v<T> && std::is_integral_v<T> && !std::is_same_v<T, bool>;

        template<class TInt>
        [[nodiscard]] inline std::enable_if_t<is_signed_integer_v<TInt>, ParseError> ParseImpl(TInt& result) const noexcept
        {
            int64_t temp;
            auto ret = TryToIntImpl(temp);
            if (ret != ParseErrorCode::None)
            {
                return ret;
            }

            result = static_cast<TInt>(temp);
            if (result != temp)
            {
                return ParseErrorCode::Overflow;
            }

            return ParseErrorCode::None;
        }

        template<class TInt>
        [[nodiscard]] inline std::enable_if_t<is_unsigned_integer_v<TInt>, ParseError> ParseImpl(TInt& result) const noexcept
        {
            uint64_t temp;
            auto ret = TryToUIntImpl(temp);
            if (ret != ParseErrorCode::None)
            {
                return ret;
            }

            result = static_cast<TInt>(temp);
            if (result != temp)
            {
                return ParseErrorCode::Overflow;
            }

            return ParseErrorCode::None;
        }

        template<class TFloat>
        [[nodiscard]] inline std::enable_if_t<std::is_floating_point_v<TFloat>, ParseError> ParseImpl(
            TFloat& result) const noexcept
        {
            double temp;
            auto ret = TryToFloatImpl(temp);
            result   = static_cast<TFloat>(temp);
            return ret;
        }

        template<class TBool>
        [[nodiscard]] inline std::enable_if_t<std::is_same_v<TBool, bool>, ParseError> ParseImpl(TBool& result) const noexcept;

        template<class T>
        [[nodiscard]] inline std::enable_if_t<ValueParser<T>::value, ParseError> ParseImpl(T& result) const noexcept
        {
            return ValueParser<T>::TryConvert(*this, result);
        }

    public:
        class Iterator
        {
            friend class StringSlice;
            friend class String;
            const TChar* m_Iter;

        public:
            using iterator_category = std::bidirectional_iterator_tag;
            using difference_type   = std::ptrdiff_t;
            using value_type        = TCodepoint;
            using pointer           = const TCodepoint*;
            using reference         = const TCodepoint&;

            inline Iterator(const TChar* iter)
                : m_Iter(iter)
            {
            }

            inline value_type operator*() const
            {
                return utf8::PeekDecode(m_Iter);
            }

            inline Iterator& operator++()
            {
                utf8::Decode(m_Iter);
                return *this;
            }

            inline Iterator operator++(int)
            {
                Iterator t = *this;
                ++(*this);
                return t;
            }

            inline Iterator& operator--()
            {
                utf8::DecodePrior(m_Iter);
                return *this;
            }

            inline Iterator operator--(int)
            {
                Iterator t = *this;
                utf8::DecodePrior(m_Iter);
                return t;
            }

            inline friend Iterator operator+(Iterator lhs, int32_t rhs)
            {
                if (rhs > 0)
                {
                    while (rhs--)
                        ++lhs;
                }
                else
                {
                    while (rhs--)
                        --lhs;
                }

                return lhs;
            }

            inline friend Iterator operator-(const Iterator& lhs, int32_t rhs)
            {
                return lhs + (-rhs);
            }

            inline friend bool operator==(const Iterator& a, const Iterator& b)
            {
                return a.m_Iter == b.m_Iter;
            };

            inline friend bool operator!=(const Iterator& a, const Iterator& b)
            {
                return a.m_Iter != b.m_Iter;
            };
        };

        inline constexpr StringSlice() noexcept
            : m_Data(nullptr)
            , m_Size(0)
        {
        }

        inline constexpr StringSlice(const TChar* data, size_t size) noexcept
            : m_Data(data)
            , m_Size(size)
        {
        }

        inline constexpr StringSlice(const std::string_view& stringView) noexcept
            : m_Data(stringView.data())
            , m_Size(stringView.size())
        {
        }

        inline constexpr StringSlice(const TChar* data) noexcept
            : m_Data(data)
            , m_Size(data == nullptr ? 0 : std::char_traits<TChar>::length(data))
        {
        }

        template<size_t S>
        inline constexpr StringSlice(const TChar (&data)[S]) noexcept
            : m_Data(data)
            , m_Size(S)
        {
        }

        inline StringSlice(Iterator begin, Iterator end) noexcept
            : m_Data(begin.m_Iter)
            , m_Size(end.m_Iter - begin.m_Iter)
        {
        }

        [[nodiscard]] inline constexpr const TChar* Data() const noexcept
        {
            return m_Data;
        }

        [[nodiscard]] inline constexpr size_t Size() const noexcept
        {
            return m_Size;
        }

        // O(N)
        [[nodiscard]] inline size_t Length() const noexcept
        {
            return utf8::Length(Data(), Size());
        }

        inline StringSlice operator()(size_t beginIndex, size_t endIndex) const
        {
            auto begin = Data();
            auto end   = Data();
            utf8::Advance(begin, beginIndex);
            utf8::Advance(end, endIndex);
            return StringSlice(begin, end - begin);
        }

        // O(1)
        [[nodiscard]] inline TChar ByteAt(size_t index) const
        {
            assert(index < Size());
            return Data()[index];
        }

        // O(N)
        [[nodiscard]] inline TCodepoint CodePointAt(size_t index) const
        {
            auto begin     = Data();
            auto end       = begin + Size() + 1;
            size_t cpIndex = 0;
            for (auto iter = begin; iter != end; utf8::Decode(iter), ++cpIndex)
            {
                if (cpIndex == index)
                    return utf8::PeekDecode(iter);
            }

            UTF_STRING_ASSERT(false);
            return 0;
        }

        [[nodiscard]] inline Iterator FindFirstOf(Iterator start, TCodepoint search) const noexcept
        {
            auto e = end();
            for (auto iter = start; iter != e; ++iter)
            {
                if (*iter == search)
                {
                    return iter;
                }
            }
            return e;
        }

        [[nodiscard]] inline Iterator FindFirstOf(TCodepoint search) const noexcept
        {
            return FindFirstOf(begin(), search);
        }

        [[nodiscard]] inline Iterator FindLastOf(TCodepoint search) const noexcept
        {
            auto e      = end();
            auto result = e;
            for (auto iter = begin(); iter != e; ++iter)
            {
                if (*iter == search)
                {
                    result = iter;
                }
            }
            return result;
        }

        [[nodiscard]] inline bool Contains(TCodepoint search) const noexcept
        {
            return FindFirstOf(search) != end();
        }

        [[nodiscard]] inline bool StartsWith(StringSlice prefix, bool caseSensitive = true) const noexcept
        {
            if (prefix.Size() > Size())
            {
                return false;
            }

            return utf8::AreEqual(Data(), prefix.Data(), prefix.Size(), prefix.Size(), caseSensitive);
        }

        [[nodiscard]] inline bool EndsWith(StringSlice suffix, bool caseSensitive = true) const noexcept
        {
            if (suffix.Size() > Size())
            {
                return false;
            }

            return utf8::AreEqual(Data() + Size() - suffix.Size(), suffix.Data(), suffix.Size(), suffix.Size(), caseSensitive);
        }

        [[nodiscard]] inline std::vector<StringSlice> Split(TCodepoint c = ' ') const
        {
            std::vector<StringSlice> result;
            auto current = begin();
            while (current != end())
            {
                auto cPos = FindFirstOf(current, c);
                result.emplace_back(current.m_Iter, cPos.m_Iter - current.m_Iter);
                current = cPos;
                if (current != end())
                {
                    ++current;
                }
            }

            return result;
        }

        [[nodiscard]] inline std::vector<StringSlice> SplitLines() const
        {
            std::vector<StringSlice> result;
            auto current = begin();
            while (current != end())
            {
                auto cPos = FindFirstOf(current, '\n');
                auto line = StringSlice(current.m_Iter, cPos.m_Iter - current.m_Iter).StripRight("\r");
                result.emplace_back(line);
                current = cPos;
                if (current != end())
                {
                    ++current;
                }
            }

            return result;
        }

        [[nodiscard]] inline StringSlice StripLeft(StringSlice chars = "\n\r\t ") const noexcept
        {
            if (Size() == 0)
            {
                return {};
            }

            auto endIter = end();
            auto result  = begin();
            for (auto iter = begin(); iter != endIter; ++iter)
            {
                if (!chars.Contains(*iter))
                {
                    break;
                }

                result = iter;
                ++result;
            }

            return { result, endIter };
        }

        [[nodiscard]] inline StringSlice StripRight(StringSlice chars = "\n\r\t ") const noexcept
        {
            if (Size() == 0)
            {
                return {};
            }

            auto beginIter = begin();
            auto result    = end();
            for (Iterator iter = --end(); iter != beginIter; --iter)
            {
                if (!chars.Contains(*iter))
                {
                    break;
                }

                result = iter;
            }

            return { beginIter, result };
        }

        [[nodiscard]] inline StringSlice Strip(StringSlice chars = "\n\r\t ") const noexcept
        {
            return StripLeft(chars).StripRight(chars);
        }

        [[nodiscard]] inline int Compare(const StringSlice& other) const noexcept
        {
            return utf8::Compare(Data(), other.Data(), Size(), other.Size());
        }

        [[nodiscard]] inline bool IsEqualTo(const StringSlice& other, bool caseSensitive = true) const noexcept
        {
            return utf8::AreEqual(Data(), other.Data(), Size(), other.Size(), caseSensitive);
        }

        template<class T>
        [[nodiscard]] inline ParseError TryParse(T& result) const
        {
            return ParseImpl(result);
        }

        [[nodiscard]] inline Iterator begin() const noexcept
        {
            return Iterator(Data());
        }

        [[nodiscard]] inline Iterator end() const noexcept
        {
            return Iterator(Data() + Size());
        }
    };

    inline bool operator==(const StringSlice& lhs, const StringSlice& rhs) noexcept
    {
        return lhs.Size() == rhs.Size() && lhs.Compare(rhs) == 0;
    }

    inline bool operator!=(const StringSlice& lhs, const StringSlice& rhs) noexcept
    {
        return !(lhs == rhs);
    }

    inline bool operator<(const StringSlice& lhs, const StringSlice& rhs) noexcept
    {
        return lhs.Compare(rhs) < 0;
    }

    inline bool operator>(const StringSlice& lhs, const StringSlice& rhs) noexcept
    {
        return lhs.Compare(rhs) > 0;
    }

    inline bool operator<=(const StringSlice& lhs, const StringSlice& rhs) noexcept
    {
        return lhs.Size() == rhs.Size() && lhs.Compare(rhs) <= 0;
    }

    inline bool operator>=(const StringSlice& lhs, const StringSlice& rhs) noexcept
    {
        return lhs.Size() == rhs.Size() && lhs.Compare(rhs) >= 0;
    }

    template<class TBool>
    std::enable_if_t<std::is_same_v<TBool, bool>, ParseError> StringSlice::ParseImpl(TBool& result) const noexcept
    {
        if (*this == "true" || *this == "1")
        {
            result = true;
            return ParseErrorCode::None;
        }
        if (*this == "false" || *this == "0")
        {
            result = false;
            return ParseErrorCode::None;
        }

        return ParseErrorCode::InvalidSyntax;
    }

    inline ParseError StringSlice::TryToUIntImpl(uint64_t& result) const
    {
        if (Size() == 0)
        {
            return ParseErrorCode::UnexpectedEnd;
        }

        const TChar* ptr    = Data();
        const TChar* endPtr = Data() + Size();

        result = 0;
        while (true)
        {
            if (ptr == endPtr)
            {
                return ParseErrorCode::None;
            }

            if (*ptr <= '9' && *ptr >= '0')
            {
                result *= 10;
                result += *ptr - '0';
            }
            else
            {
                return { ParseErrorCode::InvalidSyntax, static_cast<size_t>(ptr - Data()) };
            }

            ++ptr;
        }
    }

    inline ParseError StringSlice::TryToIntImpl(int64_t& result) const
    {
        if (Size() == 0)
        {
            return ParseErrorCode::UnexpectedEnd;
        }

        const TChar* ptr    = Data();
        const TChar* endPtr = Data() + Size();
        auto isNegative     = false;

        if (*ptr == '-')
        {
            isNegative = true;
            ++ptr;
        }

        if (endPtr - ptr == 0)
        {
            return { ParseErrorCode::UnexpectedEnd, 1 };
        }

        result = 0;
        while (true)
        {
            if (ptr == endPtr)
            {
                if (isNegative)
                {
                    result = -result;
                }

                return ParseErrorCode::None;
            }

            if (*ptr <= '9' && *ptr >= '0')
            {
                result *= 10;
                result += *ptr - '0';
            }
            else
            {
                return { ParseErrorCode::InvalidSyntax, static_cast<size_t>(ptr - Data()) };
            }

            ++ptr;
        }
    }

    inline ParseError StringSlice::TryToFloatImpl(double& result) const
    {
        if (Size() < 1)
        {
            return ParseErrorCode::UnexpectedEnd;
        }

        auto foundDot = false;

        auto slice = *this;
        if (*slice.Data() == '-')
        {
            slice = StringSlice(slice.Data() + 1, slice.Size() - 1);
        }

        size_t position = 0;
        for (auto c : slice)
        {
            if (c == '.')
            {
                if (foundDot)
                {
                    return { ParseErrorCode::InvalidSyntax, position };
                }

                foundDot = true;
            }
            else if (c > '9' || c < '0')
            {
                return { ParseErrorCode::InvalidSyntax, position };
                continue;
            }

            ++position;
        }

        result = std::strtod(Data(), nullptr);
        return ParseErrorCode::None;
    }
} // namespace utfstring

namespace std
{
    inline ostream& operator<<(ostream& stream, utfstring::StringSlice str)
    {
        return stream << std::string_view(str.Data(), str.Size());
    }

    template<>
    struct hash<utfstring::StringSlice>
    {
        inline size_t operator()(const utfstring::StringSlice& str) const noexcept
        {
            std::hash<std::string_view> hasher;
            return hasher(std::string_view(str.Data(), str.Size()));
        }
    };
} // namespace std
