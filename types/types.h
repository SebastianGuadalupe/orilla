#pragma once

#include <string>
#include <unordered_map>

enum class TypeKeyword { Int, Float, Double, String, Bool };

const std::unordered_map<std::string, TypeKeyword> stringToTypeMap = {
    {"Int", TypeKeyword::Int},
    {"Float", TypeKeyword::Float},
    {"Double", TypeKeyword::Double},
    {"String", TypeKeyword::String},
    {"Bool", TypeKeyword::Bool}};

const std::unordered_map<TypeKeyword, std::string> typeToStringMap = {
    {TypeKeyword::Int, "Int"},
    {TypeKeyword::Float, "Float"},
    {TypeKeyword::Double, "Double"},
    {TypeKeyword::String, "String"},
    {TypeKeyword::Bool, "Bool"}};
