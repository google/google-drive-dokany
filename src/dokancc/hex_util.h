#ifndef DOKAN_HEX_UTIL_H_
#define DOKAN_HEX_UTIL_H_

#include <ios>
#include <ostream>
#include <type_traits>

namespace dokan {

// stream wrapper; outputs an integer type as if with format string "0x%X".
template <typename T, typename Enable = void>
struct Hex {
  const T& val;
  explicit Hex(const T& val) : val(val) {}

  inline const T& value() const {
    return val;
  }
};
template <typename T>
struct Hex<T, std::enable_if_t<std::is_pointer_v<T>>> {
  const T& val;
  explicit Hex(const T& val) : val(val) {}

  inline uintptr_t value() const {
    return reinterpret_cast<uintptr_t>(val);
  }
};

template <typename T> Hex(const T& val) -> Hex<T>;

template <typename T>
std::ostream& operator<<(std::ostream& os, const Hex<T>& hex) {
  const auto prev_width = os.width(0);
  os << "0x";
  os.width(prev_width);

  const auto prev_flags = os.flags();
  const char prev_fill = os.fill('0');

  os << std::hex << std::uppercase << std::noshowbase << hex.value();

  os.fill(prev_fill);
  os.flags(prev_flags);
  return os;
}

// stream wrapper; outputs an integer type as if with format string "0x%X",
// padded to the appropriate width with 0's.
template <typename T, typename Enable = void>
struct PaddedHex {
  const T& val;
  explicit PaddedHex(const T& val) : val(val) {}

  inline const T& value() const {
    return val;
  }
};
template <typename T>
struct PaddedHex<T, std::enable_if_t<std::is_pointer_v<T>>> {
  const T& val;
  explicit PaddedHex(const T& val) : val(val) {}

  inline uintptr_t value() const {
    return reinterpret_cast<uintptr_t>(val);
  }
};

template <typename T> PaddedHex(const T& val) -> PaddedHex<T>;

template <typename T>
std::ostream& operator<<(std::ostream& os, const PaddedHex<T>& hex) {
  const auto prev_width = os.width(0);
  os << "0x";

  const auto prev_flags = os.flags();
  const char prev_fill = os.fill('0');

  os.width(sizeof(decltype(hex.value())) * 2);
  os << std::hex << std::uppercase << std::noshowbase << hex.value();

  os.fill(prev_fill);
  os.flags(prev_flags);
  os.width(prev_width);
  return os;
}

}  // namespace dokan

#endif  // DOKAN_HEX_UTIL_H_
