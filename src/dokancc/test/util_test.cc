/*
  Dokan : user-mode file system library for Windows

  Copyright (C) 2018 Google, Inc.
  http://dokan-dev.github.io

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.
*/

// Unit test for the functions in util.h.

#include "util.h"
#include "test_logger.h"

#include <Windows.h>

#include <mutex>  // NOLINT
#include <thread>  // NOLINT
#include <vector>

#include "gtest.h"

namespace dokan {
namespace test {
namespace {

bool FileNameMatchesPattern(const std::wstring& name,
                            const std::wstring& pattern) {
  util::FileNameMatcher matcher;
  EXPECT_TRUE(matcher.Init(pattern));
  return matcher.Matches(name);
}

}

TEST(UtilTest, Align) {
  EXPECT_EQ(0, util::Align(512, 0));
  EXPECT_EQ(512, util::Align(1, 512));
  EXPECT_EQ(512, util::Align(512, 1));
  EXPECT_EQ(1024, util::Align(512, 513));
  EXPECT_EQ(512, util::Align(512, 512));
  EXPECT_EQ(1024, util::Align(512, 1024));
}

TEST(UtilTest, SanitizePath_Empty) {
  EXPECT_EQ(L"", util::SanitizePath(L""));
}

TEST(UtilTest, SanitizePath_DoubleBackslashPrefix) {
  EXPECT_EQ(L"\\", util::SanitizePath(L"\\\\"));
  EXPECT_EQ(L"\\f", util::SanitizePath(L"\\\\f"));
  EXPECT_EQ(L"\\Foo bar\\xyz", util::SanitizePath(L"\\\\Foo bar\\xyz"));
}

TEST(UtilTest, SanitizePath_TerminatingBackslash) {
  EXPECT_EQ(L"\\a", util::SanitizePath(L"\\a\\"));
  EXPECT_EQ(L"\\a\\", util::SanitizePath(L"\\a\\\\"));
  EXPECT_EQ(L"\\Foo bar\\xyz", util::SanitizePath(L"\\Foo bar\\xyz\\"));
}

TEST(UtilTest, StripLastPathComponent_OnlyComponent) {
  EXPECT_EQ(L"\\", util::StripLastPathComponent(L"\\"));
  EXPECT_EQ(L"\\", util::StripLastPathComponent(L"\\foo"));
  EXPECT_EQ(L"\\", util::StripLastPathComponent(L"\\Foo bar"));
}

TEST(UtilTest, StripLastPathComponent_FromMultiComponents) {
  EXPECT_EQ(L"\\a", util::StripLastPathComponent(L"\\a\\b"));
  EXPECT_EQ(L"\\Foo bar", util::StripLastPathComponent(L"\\Foo bar\\abc xyz"));
}

TEST(UtilTest, ReplaceLastPathComponent_OnlyComponent) {
  EXPECT_EQ(L"\\bar", util::ReplaceLastPathComponent(L"\\foo", L"bar"));
  EXPECT_EQ(L"\\~Foo bar", util::ReplaceLastPathComponent(
      L"\\foo_ bar", L"~Foo bar"));
}

TEST(UtilTest, ReplaceLastPathComponent_MultiComponents) {
  EXPECT_EQ(L"\\foo\\bar", util::ReplaceLastPathComponent(
      L"\\foo\\foo", L"bar"));
  EXPECT_EQ(L"\\Foo bar\\x yz", util::ReplaceLastPathComponent(
      L"\\Foo bar\\a b c d", L"x yz"));
}

TEST(UtilTest, SplitAlternateStreamName_Empty) {
  std::wstring path;
  std::wstring stream_name;
  util::SplitAlternateStreamName(&path, &stream_name);
  EXPECT_EQ(L"", path);
  EXPECT_EQ(L"", stream_name);
}

TEST(UtilTest, SplitAlternateStreamName_NoStream) {
  std::wstring path = L"\\foo\\b Ar_$.txt";
  std::wstring stream_name;
  util::SplitAlternateStreamName(&path, &stream_name);
  EXPECT_EQ(L"\\foo\\b Ar_$.txt", path);
  EXPECT_EQ(L"", stream_name);
}

TEST(UtilTest, SplitAlternateStreamName_Stream) {
  std::wstring path = L"\\foo\\b Ar_$.txt:foo $_bAr";
  std::wstring stream_name;
  util::SplitAlternateStreamName(&path, &stream_name);
  EXPECT_EQ(L"\\foo\\b Ar_$.txt", path);
  EXPECT_EQ(L"foo $_bAr", stream_name);
}

TEST(UtilTest, SplitAlternateStreamName_StreamWithType) {
  std::wstring path = L"\\foo\\b Ar_$.txt:foo $_bAr: x y Z ";
  std::wstring stream_name;
  util::SplitAlternateStreamName(&path, &stream_name);
  EXPECT_EQ(L"\\foo\\b Ar_$.txt", path);
  EXPECT_EQ(L"foo $_bAr: x y Z ", stream_name);
}

TEST(UtilTest, MakeDevicePath_Root) {
  EXPECT_EQ(L"\\\\?\\Volume{deadbeef}",
            util::MakeDevicePath(L"\\Volume{deadbeef}"));
}

TEST(UtilTest, MakeDevicePath_Subpath) {
  EXPECT_EQ(L"\\\\?\\Volume{deadbeef}\\Foo bar",
      util::MakeDevicePath(L"\\Volume{deadbeef}", L"Foo bar"));
  EXPECT_EQ(L"\\\\?\\Volume{deadbeef}\\Foo bar",
      util::MakeDevicePath(L"\\Volume{deadbeef}", L"\\Foo bar"));
}

TEST(UtilTest, FileNameMatchesPattern_Basic) {
  EXPECT_TRUE(FileNameMatchesPattern(L"", L""));
  EXPECT_TRUE(FileNameMatchesPattern(L"a", L"A"));
  EXPECT_FALSE(FileNameMatchesPattern(L"a", L"B"));
  EXPECT_TRUE(FileNameMatchesPattern(L"a-", L"A-"));
  EXPECT_TRUE(FileNameMatchesPattern(L"-$ ab C.txt", L"-$ AB C.TXT"));
  EXPECT_FALSE(FileNameMatchesPattern(L"-$ ab C.txt", L"-$ AB C.TXT1"));
  EXPECT_FALSE(FileNameMatchesPattern(L"-$ ab C.txt1", L"-$ AB C.TXT"));
}

TEST(UtilTest, FileNameMatchesPattern_Star) {
  EXPECT_FALSE(FileNameMatchesPattern(L"", L"*"));
  EXPECT_TRUE(FileNameMatchesPattern(L"a", L"*"));
  EXPECT_TRUE(FileNameMatchesPattern(L"A", L"*"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aB c", L"*"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aB c.txt", L"*"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aB c.txt", L"*.*"));
  EXPECT_FALSE(FileNameMatchesPattern(L"aB c.txt", L".*"));
}

TEST(UtilTest, FileNameMatchesPattern_Question) {
  EXPECT_FALSE(FileNameMatchesPattern(L"", L"?"));
  EXPECT_TRUE(FileNameMatchesPattern(L"a", L"?"));
  EXPECT_TRUE(FileNameMatchesPattern(L"A", L"?"));
  EXPECT_FALSE(FileNameMatchesPattern(L"aB", L"?"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aB", L"A?"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aB", L"?B"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aBc", L"A?C"));
  EXPECT_TRUE(FileNameMatchesPattern(L"aBc", L"??C"));
}

TEST(UtilTest, FileNameMatchesPattern_Dos) {
  // Hacky characters that the kernel uses to get DOS semantics for characters
  // that normally behave slightly differently now.
#define DOS_STAR L"<"
#define DOS_QUESTION L">"
#define DOS_DOT L"\""

  EXPECT_TRUE(FileNameMatchesPattern(L"a", DOS_STAR));
  EXPECT_TRUE(FileNameMatchesPattern(L"a", L"A" DOS_DOT));
  EXPECT_TRUE(FileNameMatchesPattern(L"a", L"A" DOS_DOT DOS_STAR));
  EXPECT_FALSE(FileNameMatchesPattern(L"a.txt", DOS_STAR));
  EXPECT_TRUE(FileNameMatchesPattern(
      L"a.txt", DOS_STAR DOS_DOT DOS_STAR));
  EXPECT_TRUE(FileNameMatchesPattern(
      L"a.txt", DOS_QUESTION DOS_QUESTION DOS_DOT DOS_STAR));
}

TEST(UtilTest, CheckDriverVersion_Failure) {
  TestLogger logger;
  GUID guid = {0xdeadbeef};
  EXPECT_FALSE(util::CheckDriverVersion(guid, &logger));
  EXPECT_TRUE(logger.HasErrors());
}

TEST(UtilTest, CheckDriverVersion_Success) {
  TestLogger logger;
  GUID guid = DOKAN_DRIVER_VERSION;
  EXPECT_TRUE(util::CheckDriverVersion(guid, &logger));
  EXPECT_FALSE(logger.HasErrors());
}

TEST(UtilTest, SetAndNotify) {
  std::mutex mutex;
  std::condition_variable var;
  bool flag = false;
  std::thread thread([&] {
    Sleep(300);  // Make it so the wait doesn't trivially exit.
    util::SetAndNotify(&mutex, &var, &flag);
  });
  {
    std::unique_lock<std::mutex> lock(mutex);
    var.wait(lock, [&] { return flag; });
    EXPECT_TRUE(flag);
  }
  thread.join();
}

TEST(UtilTest, TimeToLargeInteger) {
  FILETIME t;
  t.dwHighDateTime = 0x12345678;
  t.dwLowDateTime = 0x9abcdef0;
  LARGE_INTEGER i;
  util::TimeToLargeInteger(t, &i);
  EXPECT_EQ(0x123456789abcdef0, i.QuadPart);
}

TEST(UtilTest, LargeIntegerToTime) {
  LARGE_INTEGER i;
  i.QuadPart = 0x123456789abc;
  FILETIME t = {0};
  util::LargeIntegerToTime(i, &t);
  EXPECT_EQ(0x56789abc, t.dwLowDateTime);
  EXPECT_EQ(0x1234, t.dwHighDateTime);
}

TEST(UtilTest, Narrow) {
  std::string temp;
  EXPECT_TRUE(util::Narrow(L"", &temp));
  EXPECT_EQ("", temp);
  EXPECT_TRUE(util::Narrow(L"foobar", &temp));
  EXPECT_EQ("foobar", temp);
  EXPECT_TRUE(util::Narrow(L"Hwæt!", &temp));
  EXPECT_EQ("Hw\xc3\x83\xc2\xa6t!", temp);
  static const wchar_t kBadData[] = {0xd8ff, 0xffff, 0, 0};
  EXPECT_FALSE(util::Narrow(kBadData, &temp));
}

}  // namespace test
}  // namespace dokan
