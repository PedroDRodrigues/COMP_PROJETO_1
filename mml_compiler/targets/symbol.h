#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    long _value; 
    int _qualifier;
    bool _is_external;
    bool _is_foreign;
    bool _is_main;

    int _offset = 0;

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, long value, int qualifier) :
        _type(type), _name(name), _value(value), _qualifier(qualifier), _is_external(false), _is_foreign(false), _is_main(false) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    
    const std::string &name() const {
      return _name;
    }
    
    long value() const {
      return _value;
    }
    
    long value(long v) {
      return _value = v;
    }

    int qualifier() const {
      return _qualifier;
    }

    void set_external(bool external) {
      _is_external = external;
    }

    bool is_external()  {
      return _is_external;
    }

    void set_foreign(bool foreign) {
      _is_foreign = foreign;
    }

    bool is_foreign() {
      return _is_foreign;
    }

    void set_main(bool main) {
      _is_main = main;
    }

    bool is_main() {
      return _is_main;
    }

    int offset() {
      return _offset;
    }

    bool global() {
      return _offset == 0;
    }

    void set_offset(int offset) {
      _offset = offset;
    }

  };

  inline auto make_symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name, long value, int qualifier) {
    return std::make_shared<symbol>(type, name, value, qualifier);
  }

} // mml

#endif
