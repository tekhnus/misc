#ifndef TAGZ_BASE_TAG_HPP
#define TAGZ_BASE_TAG_HPP

#include <string>
#include <initializer_list>

namespace tagz {
namespace base {

class tag
{
public:
    tag(const std::string &prefix, const std::string &suffix, std::initializer_list<std::string> content);
    operator const std::string&();
private:
    std::string value;
};

} // namespace base
} // namespace tagz

#endif // TAGZ_BASE_TAG_HPP
