/*
 * LegacyClonk
 *
 * Copyright (c) 2019-2020, The LegacyClonk Team and contributors
 *
 * Distributed under the terms of the ISC license; see accompanying file
 * "COPYING" for details.
 *
 * "Clonk" is a registered trademark of Matthes Bender, used with permission.
 * See accompanying file "TRADEMARK" for details.
 *
 * To redistribute this file separately, substitute the full license texts
 * for the above references.
 */

#pragma once
#include "C4Value.h"
#include "C4ValueStandardRefCountedContainer.h"

#include <unordered_map>
#include <forward_list>
#include <list>
#include <memory>

class C4ValueHash : public C4ValueStandardRefCountedContainer<C4ValueHash>
{
public:
	using key_type = C4Value;
	using mapped_type = C4Value;

private:
	struct MapEntry
	{
		C4Value *value;
		std::list<const C4Value *>::iterator keyOrderIterator;
	};

	std::unordered_map<key_type, MapEntry> map;
	std::forward_list<C4Value *> emptyValues;

	// we need a defined order for network sync
	std::list<const C4Value *> keyOrder;

public:

	class Iterator
	{
		using pair_type = std::pair<const C4Value &, C4Value &>;
		using iterator_type = decltype(keyOrder.begin());
		iterator_type it, end;
		C4ValueHash *map;
		std::unique_ptr<pair_type> current;

		void update();

	public:
		Iterator(C4ValueHash *map, iterator_type it, iterator_type end);

		Iterator &operator++();
		pair_type &operator*();
		bool operator==(const Iterator &other);
		bool operator!=(const Iterator &other);
	};

	C4ValueHash();
	C4ValueHash(const C4ValueHash &other);

	~C4ValueHash();

	virtual void CompileFunc(StdCompiler *pComp) override;
	virtual void DenumeratePointers() override;

	C4ValueHash &operator=(const C4ValueHash &other);
	bool operator==(const C4ValueHash &other) const;
	virtual bool hasIndex(const C4Value &key) const override;
	virtual C4Value &operator[](const C4Value &key) override;
	virtual const C4Value &operator[](const C4Value &key) const;
	Iterator begin();
	Iterator end();

	bool contains(const C4Value &key) const;
	void removeValue(C4Value *value);
	auto size() const { return map.size(); }
	void clear();
};
