/*
 * LegacyClonk
 *
 * Copyright (c) RedWolf Design
 * Copyright (c) 2017-2019, The LegacyClonk Team and contributors
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
#include <set>
#include <string>

class C4EnergyBar
{
public:
	int32_t value;
	int32_t max;
	bool visible;

	C4EnergyBar(): value(0), max(1000000), visible(true) {}
	C4EnergyBar(int32_t _value, int32_t _max, bool _visible);
	bool operator==(const C4EnergyBar &rhs) const {
		return value == rhs.value && max == rhs.max && visible == rhs.visible;
	}

	void CompileFunc(StdCompiler *pComp) {
		pComp->Value(mkNamingAdapt(value, "Value", 0));
		pComp->Value(mkNamingAdapt(max, "Max", 1000000));
		pComp->Value(mkNamingAdapt(visible, "Visible", true));
	}
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
	void SetEnergyBar(std::string name, int32_t value, int32_t max = 0);
	void SetEnergyBarVisible(std::string name, bool visible);

private:
	C4EnergyBar* BarVal(std::string name);
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
	C4EnergyBarDef(const char *_name, const char *_file, const std::shared_ptr<C4FacetExID> &_gfx, int32_t _index, int32_t _physical = 0);
  ~C4EnergyBarDef();

	bool operator==(const C4EnergyBarDef &rhs) const;

	static int32_t PhysicalFromName(std::string name);
	static int32_t DefaultHide(int32_t physical);
	static int32_t DefaultIndex(int32_t physical);

	std::size_t GetHash() const;

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

	void CompileFunc(StdCompiler *pComp) {
		pComp->Value(mkNamingAdapt(name, "Name"));
		pComp->Value(mkNamingAdapt(physical, "Physical", 0));
		pComp->Value(mkNamingAdapt(hide, "Hide", 1));
		pComp->Value(mkNamingAdapt(gfx, "Gfx"));
		pComp->Value(mkNamingAdapt(index, "Index"));
		pComp->Value(mkNamingAdapt(advance, "Advance", true));
		pComp->Value(mkNamingAdapt(value_index, "ValueIndex", -1));
		pComp->Value(mkNamingAdapt(value, "Value", 0));
		pComp->Value(mkNamingAdapt(max, "Max", 1000000));
		pComp->Value(mkNamingAdapt(visible, "Visible", true));
		// gfx and scale are restored from def.gfxs
	}
};

namespace {
	template <typename Map>
	bool map_compare (Map const &lhs, Map const &rhs) {
		return lhs.size() == rhs.size()
			&& std::equal(lhs.begin(), lhs.end(),
										rhs.begin());
	}
}

class C4EnergyBarsDef
{
public:
	struct Gfx {
		std::string key;
		std::string file;
		int32_t amount;
		int32_t scale;
		Gfx():key(), file(), amount(0), scale(0) {}
		Gfx(std::string k, std::string f, int32_t _a, int32_t _s):key(k), file(f), amount(_a), scale(_s) {}
		bool operator==(const Gfx &rhs) const {return key == rhs.key && file == rhs.file && amount == rhs.amount && scale == rhs.scale;}

		void CompileFunc(StdCompiler *pComp) {
			pComp->Value(mkNamingAdapt(key, "Key"));
			pComp->Value(mkNamingAdapt(file, "File"));
			pComp->Value(mkNamingAdapt(amount, "Amount"));
			pComp->Value(mkNamingAdapt(scale, "Scale"));
		}
	};
	// Gfx: amount scale
	// using Gfx   = std::tuple<int32_t, int32_t>;
	using Gfxs  = std::map<std::string, Gfx>;
	using Bars  = std::vector<C4EnergyBarDef>;
	using Names = std::unordered_map<std::string, int32_t>;

	C4EnergyBarsDef();
	C4EnergyBarsDef(const Gfxs &_gfxs, const Bars &_bars);
	C4EnergyBarsDef(const Gfxs &_gfxs, const Bars &_bars, const Names &_names);

	C4EnergyBarsDef(const C4EnergyBarsDef &other) = default;
	C4EnergyBarsDef(C4EnergyBarsDef& other) = default;
	C4EnergyBarsDef(C4EnergyBarsDef&& other) = default;
	~C4EnergyBarsDef();

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
	C4EnergyBarsUniquifier() {}
	~C4EnergyBarsUniquifier() {}

	std::shared_ptr<C4EnergyBars> DefaultBars()
	{
		if(!defaultbars) {
			const char* file = "EnergyBars";
			auto gfxs = C4EnergyBarsDef::Gfxs{{file, C4EnergyBarsDef::Gfx(file, file, 3, 100)}};
			auto gfx = GetFacet(gfxs, file);
			auto def = UniqueifyDefinition(
				new C4EnergyBarsDef(
					gfxs,
					C4EnergyBarsDef::Bars{
						C4EnergyBarDef("Energy", file, gfx, 0, C4EnergyBarDef::EBP_Energy),
						C4EnergyBarDef("Magic",  file, gfx, 1, C4EnergyBarDef::EBP_Magic),
						C4EnergyBarDef("Breath", file, gfx, 2, C4EnergyBarDef::EBP_Breath)
			}));
			defaultbars = Instantiate(def);
		}

		return defaultbars;
	}

	void RemoveDef(const C4EnergyBarsDef &def) {
		definitions.erase(def);
	}

	void CompileFunc(StdCompiler *pComp);
	std::shared_ptr<C4EnergyBars> DefineEnergyBars(C4ValueHash* graphics, C4ValueArray *definition);

	std::shared_ptr<C4FacetExID>     GetFacet(C4EnergyBarsDef::Gfxs &gfx, const char *file);
	std::shared_ptr<C4EnergyBarsDef> UniqueifyDefinition(C4EnergyBarsDef *definition);
	std::shared_ptr<C4EnergyBars>    Instantiate(std::shared_ptr<C4EnergyBarsDef> definition);

private:
	bool ProcessGraphics(C4ValueHash &map, C4EnergyBarsDef::Gfxs &gfx);
	bool ProcessGroup(int32_t &value_index, C4EnergyBarsDef::Gfxs &graphics, const C4ValueArray &group, C4EnergyBarsDef::Bars &bars, bool advanceAlways);
	bool ProcessEnergyBar(int32_t &value_index, C4EnergyBarsDef::Gfxs &graphics, const C4ValueHash &bar, C4EnergyBarsDef::Bars &bars, bool advance);
	bool PopulateNamesFromValues(const C4EnergyBarsDef::Bars &bars, C4EnergyBarsDef::Names &names);

	std::unordered_map<std::string, std::weak_ptr<C4FacetExID>>         graphics;
	std::unordered_map<C4EnergyBarsDef, std::weak_ptr<C4EnergyBarsDef>> definitions;

	std::shared_ptr<C4EnergyBars> defaultbars;
};

class C4EnergyBarsAdapt
{
protected:
	std::shared_ptr<C4EnergyBars> &pBars;

public:
	C4EnergyBarsAdapt(std::shared_ptr<C4EnergyBars> &_bars): pBars(_bars) {}
	void CompileFunc(StdCompiler *pComp);

	// Default checking / setting
	bool operator==(std::shared_ptr<C4EnergyBars> pDefault) { return pBars == pDefault; }
	void operator=(std::shared_ptr<C4EnergyBars> pDefault)  { pBars = pDefault; }
};

