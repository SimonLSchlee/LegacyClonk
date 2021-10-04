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
	void SetEnergyBar(C4AulContext *cthr, const std::string &name, int32_t value, int32_t max = 0);
	void SetEnergyBarVisible(C4AulContext *cthr, const std::string &name, bool visible);

private:
	C4EnergyBar* BarVal(C4AulContext *cthr, const char *functionName, const std::string &name);
};

class C4EnergyBarDef
{
public:
	enum Physical {
		EBP_None   = 0,
		EBP_Energy = 1,
		EBP_Magic  = 2,
		EBP_Breath = 3,
		EBP_All = EBP_Energy | EBP_Magic | EBP_Breath
	};
	enum Hide {
		EBH_Never = 0,
		EBH_HideHUDBars = 1,
		EBH_Empty = 2,
		EBH_Full = 4,
		EBH_EmptyFull = EBH_Empty | EBH_Full,
		EBH_All = EBH_EmptyFull | EBH_HideHUDBars
	};

public:
  C4EnergyBarDef();
	C4EnergyBarDef(std::string_view _name, std::string_view _file, const std::shared_ptr<C4FacetExID> &_gfx, int32_t _index, Physical _physical = EBP_None);

	bool operator==(const C4EnergyBarDef &rhs) const;

	static Hide DefaultHide(Physical physical);
	static int32_t DefaultIndex(Physical physical);

	std::size_t GetHash() const;
	void CompileFunc(StdCompiler *comp);

public:
	std::string name;
	Physical physical;
	Hide hide;

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

inline C4EnergyBarDef::Hide operator|(C4EnergyBarDef::Hide a, C4EnergyBarDef::Hide b);


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
	C4EnergyBarsDef(C4EnergyBarsDef&& other) = default;
	C4EnergyBarsDef& operator=(const C4EnergyBarsDef& other) = default;
	C4EnergyBarsDef& operator=(C4EnergyBarsDef&& other) = default;

	static void PopulateNamesFromValues(const std::function<void(StdStrBuf)> &error, const Bars &bars, Names &names);

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

	std::shared_ptr<C4FacetExID>     GetFacet(const std::function<void(StdStrBuf)> &error, const C4EnergyBarsDef::Gfxs &gfx, std::string_view file);
	std::shared_ptr<C4EnergyBarsDef> UniqueifyDefinition(C4EnergyBarsDef *definition);
	std::shared_ptr<C4EnergyBars>    Instantiate(std::shared_ptr<C4EnergyBarsDef> definition);
	std::shared_ptr<C4EnergyBars>    DefineEnergyBars(C4AulContext *cthr, C4ValueHash &graphics, const C4ValueArray &definition);

private:
	void ProcessGraphics(C4AulContext *cthr, C4ValueHash &map, C4EnergyBarsDef::Gfxs &gfx);
	void ProcessGroup(C4AulContext *cthr, int32_t &value_index, const C4EnergyBarsDef::Gfxs &graphics, const C4ValueArray &group, C4EnergyBarsDef::Bars &bars, bool advanceAlways);
	void ProcessEnergyBar(C4AulContext *cthr, int32_t &value_index, const C4EnergyBarsDef::Gfxs &graphics, const C4ValueHash &bar, C4EnergyBarsDef::Bars &bars, bool advance);

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

