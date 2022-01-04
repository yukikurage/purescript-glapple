'use strict';

// shallow copy
exports.copy = (record) => {
  return Object.assign({}, record);
}

exports.unsafeInsert = (key) => (value) => (record) => {
  record[key] = value;
  return record;
}

exports.unsafeModify = (key) => (func) => (record) => {
  record[key] = func(record[key]);
  return record;
}

exports.unsafeDelete = (key) => (record) =>{
  delete record[key];
  return record;
}

exports.unsafeRename = (oldKey) => (newKey) => (record) => {
  record[newKey] = record[oldKey];
  delete record[oldKey];
  return record;
}
