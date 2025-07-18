## Unreleased changes

## 2.13.1.4

* Support `persistent-2.17`

## 2.13.1.3

* Support persistent-2.14 with `SafeToInsert` class

## 2.13.1.2

* [#1367](https://github.com/yesodweb/persistent/pull/1367),
  [#1366](https://github.com/yesodweb/persistent/pull/1367),
  [#1338](https://github.com/yesodweb/persistent/pull/1338),
  [#1335](https://github.com/yesodweb/persistent/pull/1335)
    * Support GHC 9.2
* Test migration idempotency on additional integer types [#1359](https://github.com/yesodweb/persistent/pull/1359)

## 2.13.1.0

* Support `persistent-2.13.3.0` [#1341](https://github.com/yesodweb/persistent/pull/1341)

## 2.13.0.4

* `aeson-2.0` support

## 2.13.0.3

* Accidentally released 2.13.0.2 with some testing changes.

## 2.13.0.2

* [#1275](https://github.com/yesodweb/persistent/pull/1275)
    * Add a test for SafeToRemove fields

## 2.13.0.1

* [#1265](https://github.com/yesodweb/persistent/pull/1265)
    * Support GHC 9

## 2.13.0.0

* [#1225](https://github.com/yesodweb/persistent/pull/1225)
    * Support `persistent-2.13` changes for SqlBackend being made internal.

## 2.12.0.0

* Decomposed `HaskellName` into `ConstraintNameHS`, `EntityNameHS`, `FieldNameHS`. Decomposed `DBName` into `ConstraintNameDB`, `EntityNameDB`, `FieldNameDB` respectively. [#1174](https://github.com/yesodweb/persistent/pull/1174)

## 2.0.3.5

* Tighter version bounds on `persistent` and `persistent-template`.
    * [#1155](https://github.com/yesodweb/persistent/pull/1155)

## 2.0.3.4

* lots of stuff actually :\ should probably start tracking this more!

## 2.0.3.3

* Fix RawSqlTest, which could fail non-deterministically for Postgres [#1139](https://github.com/yesodweb/persistent/pull/1139)

## 2.0.3.2

* Remove unnecessary deriving of Typeable [#1114](https://github.com/yesodweb/persistent/pull/1114)

## 2.0.3.1

* Compatibility with latest persistent-template for test suite [#1002](https://github.com/yesodweb/persistent/pull/1002/files)
