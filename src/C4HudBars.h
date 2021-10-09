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

class C4HudBars;

#include "C4FacetEx.h"
#include "C4Id.h"
#include "C4Object.h"

#include <unordered_map>
#include <vector>
#include <map>
#include <memory>
#include <string>

extern int32_t counter;
extern int32_t alive;

class LifetimeHelper
{
  int32_t number;
	void Report(const char *event);
public:
  LifetimeHelper() :number(++counter) {++alive; Report("Start");}
  ~LifetimeHelper() {--alive; Report("End");}
	LifetimeHelper(const LifetimeHelper &other);
	LifetimeHelper(LifetimeHelper &&other);
};

class C4HudBar
{
public:
	int32_t value;
	int32_t max;
	bool visible;

	C4HudBar() noexcept;
	C4HudBar(int32_t _value, int32_t _max, bool _visible) noexcept;
	bool operator==(const C4HudBar &rhs) const noexcept;

	void CompileFunc(StdCompiler *comp);
};

class C4HudBarsDef;
class C4Object;

class C4HudBars
{
public:
	std::shared_ptr<const C4HudBarsDef> def;
	std::vector<C4HudBar> values;

	C4HudBars(std::shared_ptr<const C4HudBarsDef> _def) noexcept;

	void DrawHudBars(C4Facet &cgo, C4Object &obj) const noexcept;
	void SetHudBarValue(C4AulContext *cthr, const std::string &name, int32_t value, int32_t max = 0);
	void SetHudBarVisibility(C4AulContext *cthr, const std::string &name, bool visible);

private:
	C4HudBar *BarVal(C4AulContext *cthr, const char *functionName, const std::string &name);
};

class C4HudBarDef
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
  C4HudBarDef() noexcept;
	C4HudBarDef(std::string_view _name, std::string_view _file, const std::shared_ptr<C4FacetExID> &_gfx, int32_t _index, Physical _physical = EBP_None) noexcept;

	bool operator==(const C4HudBarDef &rhs) const noexcept;

	static Hide DefaultHide(Physical physical) noexcept;
	static int32_t DefaultIndex(Physical physical) noexcept;

	std::size_t GetHash() const noexcept;
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

inline const C4HudBarDef::Hide operator|(const C4HudBarDef::Hide a, const C4HudBarDef::Hide b);


class C4HudBarsDef
{
public:
	struct Gfx {
		std::string key;
		std::string file;
		int32_t amount;
		int32_t scale;
		Gfx() noexcept;
		Gfx(std::string k, std::string f, int32_t _a, int32_t _s) noexcept;
		bool operator==(const Gfx &rhs) const noexcept;

		void CompileFunc(StdCompiler *comp);
	};
	using Gfxs  = std::map<std::string, Gfx>;
	using Bars  = std::vector<C4HudBarDef>;
	using Names = std::unordered_map<std::string, int32_t>;

	C4HudBarsDef() = default;
	C4HudBarsDef(const Gfxs &_gfxs, const Bars &_bars);
	C4HudBarsDef(const Gfxs &_gfxs, const Bars &_bars, const Names &_names) noexcept;

	C4HudBarsDef(const C4HudBarsDef &other) = default;
	C4HudBarsDef(C4HudBarsDef &&other) = default;
	C4HudBarsDef &operator=(const C4HudBarsDef &other) = default;
	C4HudBarsDef &operator=(C4HudBarsDef &&other) = default;

	static void PopulateNamesFromValues(const std::function<void(StdStrBuf)> &error, const Bars &bars, Names &names);

	bool operator==(const C4HudBarsDef &rhs) const noexcept;
	std::size_t GetHash() const noexcept;

public:
	// the definiton is processed and flattened into a vector of energy bar values
	// they contain everything needed to draw the energy bars
	Gfxs  gfxs;
	Bars  bars;
	Names names;
	LifetimeHelper helper;
};

namespace std
{
	template<>
	struct hash<const C4HudBarsDef>
	{
		std::size_t operator()(const C4HudBarsDef &value) const noexcept;
	};
}


class C4HudBarsUniquifier
{
public:
	std::shared_ptr<C4HudBars>          DefaultBars();
	std::shared_ptr<C4FacetExID>        GetFacet(const std::function<void(StdStrBuf)> &error, const C4HudBarsDef::Gfxs &gfx, std::string_view file);
	std::shared_ptr<const C4HudBarsDef> UniqueifyDefinition(std::unique_ptr<C4HudBarsDef> definition);
	std::shared_ptr<C4HudBars>          Instantiate(std::shared_ptr<const C4HudBarsDef> definition);
	std::shared_ptr<C4HudBars>          DefineHudBars(C4AulContext *cthr, C4ValueHash &graphics, const C4ValueArray &definition);

private:
	void ProcessGraphics(C4AulContext *cthr, C4ValueHash &map, C4HudBarsDef::Gfxs &gfx);
	void ProcessGroup(C4AulContext *cthr, int32_t &value_index, const C4HudBarsDef::Gfxs &graphics, const C4ValueArray &group, C4HudBarsDef::Bars &bars, bool advanceAlways);
	void ProcessHudBar(C4AulContext *cthr, int32_t &value_index, const C4HudBarsDef::Gfxs &graphics, const C4ValueHash &bar, C4HudBarsDef::Bars &bars, bool advance);

  using C4HudBarsDefRef = std::reference_wrapper<const C4HudBarsDef>;
  using Definitions = std::unordered_map<C4HudBarsDefRef, std::weak_ptr<const C4HudBarsDef>, std::hash<const C4HudBarsDef>, std::equal_to<const C4HudBarsDef>>;

	std::unordered_map<std::string, std::weak_ptr<C4FacetExID>> graphics;
	Definitions definitions;

	std::shared_ptr<C4HudBars> defaultBars;
};


class C4HudBarsAdapt
{
protected:
	std::shared_ptr<C4HudBars> &bars;

public:
	C4HudBarsAdapt(std::shared_ptr<C4HudBars> &_bars): bars(_bars) {}
	void CompileFunc(StdCompiler *comp);

	// Default checking / setting
	bool operator==(std::shared_ptr<C4HudBars> pDefault) { return bars == pDefault; }
	void operator=(std::shared_ptr<C4HudBars> pDefault)  { bars = pDefault; }
};

