/*
 * LegacyClonk
 *
 * Copyright (c) RedWolf Design
 * Copyright (c) 2017-2021, The LegacyClonk Team and contributors
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

/* Allows to define custom energy bars */

#pragma once

class C4EnergyBars;

#include "C4FacetEx.h"
#include "C4Id.h"
#include "C4Object.h"

#include <unordered_map>
#include <vector>
#include <memory>
#include <string>

class C4EnergyBar
{
public:
	int32_t value;
	int32_t max;
	bool visible;

	C4EnergyBar();
	C4EnergyBar(int32_t _value, int32_t _max, bool _visible);
	bool operator==(const C4EnergyBar &rhs) const;

	void CompileFunc(StdCompiler *comp);
};


class C4EnergyBarsDef;
class C4Object;

class C4EnergyBars
{
public:
	std::shared_ptr<C4EnergyBarsDef> def;
	std::vector<C4EnergyBar> values;

	C4EnergyBars(std::shared_ptr<C4EnergyBarsDef> _def);

	void DrawEnergyBars(C4Facet &cgo, C4Object &obj);
	void SetEnergyBar(const std::string &name, int32_t value, int32_t max = 0);
	void SetEnergyBarVisible(const std::string &name, bool visible);

private:
	C4EnergyBar* BarVal(const std::string &name);
};


class C4EnergyBarDef
{
public:
	enum Physical : int32_t {
		EBP_None   = 0,
		EBP_Energy = 1,
		EBP_Magic  = 2,
		EBP_Breath = 3,
		EBP_All = EBP_Energy | EBP_Magic | EBP_Breath
	};
	enum Hide : int32_t {
		EBH_Never = 0,
		EBH_Empty = 1,
		EBH_Full = 2,
		EBH_EmptyFull = EBH_Empty | EBH_Full,
		EBH_HideHUDBars = 512,
		EBH_All = EBH_EmptyFull | EBH_HideHUDBars
	};

public:
  C4EnergyBarDef();
	C4EnergyBarDef(std::string_view _name, std::string_view _file, const std::shared_ptr<C4FacetExID> &_gfx, int32_t _index, int32_t _physical = 0);

	bool operator==(const C4EnergyBarDef &rhs) const;

	static int32_t DefaultHide(int32_t physical);
	static int32_t DefaultIndex(int32_t physical);

	std::size_t GetHash() const;
	void CompileFunc(StdCompiler *comp);

public:
	std::string name;
	int32_t physical;
	int32_t hide;

	std::string gfx;
	std::shared_ptr<C4FacetExID> facet; // calculated from gfx
	int32_t index;
	bool advance;

	int32_t value_index;
	int32_t value;
	int32_t max;
	bool visible;
	float scale; // calculated from gfx.scale
};


class C4EnergyBarsDef
{
public:
	struct Gfx {
		std::string key;
		std::string file;
		int32_t amount;
		int32_t scale;
		Gfx();
		Gfx(std::string k, std::string f, int32_t _a, int32_t _s);
		bool operator==(const Gfx &rhs) const;

		void CompileFunc(StdCompiler *comp);
	};
	using Gfxs  = std::map<std::string, Gfx>;
	using Bars  = std::vector<C4EnergyBarDef>;
	using Names = std::unordered_map<std::string, int32_t>;

	C4EnergyBarsDef() = default;
	C4EnergyBarsDef(const Gfxs &_gfxs, const Bars &_bars);
	C4EnergyBarsDef(const Gfxs &_gfxs, const Bars &_bars, const Names &_names);

	C4EnergyBarsDef(const C4EnergyBarsDef &other) = default;
	C4EnergyBarsDef(C4EnergyBarsDef& other) = default;
	C4EnergyBarsDef(C4EnergyBarsDef&& other) = default;

	static bool PopulateNamesFromValues(const Bars &bars, Names &names);
	C4EnergyBarsDef& operator=(C4EnergyBarsDef&& other) = default;

	bool operator==(const C4EnergyBarsDef &rhs) const;
	std::size_t GetHash() const;

public:
	// the definiton is processed and flattened into a vector of energy bar values
	// they contain everything needed to draw the energy bars
	Gfxs  gfxs;
	Bars  bars;
	Names names;
};

namespace std
{
	template<>
	struct hash<C4EnergyBarsDef>
	{
		std::size_t operator()(const C4EnergyBarsDef &value) const noexcept;
	};
}


class C4EnergyBarsUniquifier
{
public:
	std::shared_ptr<C4EnergyBars> DefaultBars();
	void RemoveDef(const C4EnergyBarsDef &def);

	std::shared_ptr<C4FacetExID>     GetFacet(const C4EnergyBarsDef::Gfxs &gfx, std::string_view file);
	std::shared_ptr<C4EnergyBarsDef> UniqueifyDefinition(C4EnergyBarsDef *definition);
	std::shared_ptr<C4EnergyBars>    Instantiate(std::shared_ptr<C4EnergyBarsDef> definition);
	std::shared_ptr<C4EnergyBars>    DefineEnergyBars(C4ValueHash* graphics, C4ValueArray *definition);

private:
	bool ProcessGraphics(C4ValueHash &map, C4EnergyBarsDef::Gfxs &gfx);
	bool ProcessGroup(int32_t &value_index, const C4EnergyBarsDef::Gfxs &graphics, const C4ValueArray &group, C4EnergyBarsDef::Bars &bars, bool advanceAlways);
	bool ProcessEnergyBar(int32_t &value_index, const C4EnergyBarsDef::Gfxs &graphics, const C4ValueHash &bar, C4EnergyBarsDef::Bars &bars, bool advance);

	std::unordered_map<std::string, std::weak_ptr<C4FacetExID>>         graphics;
	std::unordered_map<C4EnergyBarsDef, std::weak_ptr<C4EnergyBarsDef>> definitions;

	std::shared_ptr<C4EnergyBars> defaultBars;
};


class C4EnergyBarsAdapt
{
protected:
	std::shared_ptr<C4EnergyBars> &bars;

public:
	C4EnergyBarsAdapt(std::shared_ptr<C4EnergyBars> &_bars): bars(_bars) {}
	void CompileFunc(StdCompiler *comp);

	// Default checking / setting
	bool operator==(std::shared_ptr<C4EnergyBars> pDefault) { return bars == pDefault; }
	void operator=(std::shared_ptr<C4EnergyBars> pDefault)  { bars = pDefault; }
};

