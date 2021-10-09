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

#include <C4Include.h>
#include <C4HudBars.h>
#include <C4Value.h>
#include <C4ValueList.h>
#include <C4ValueHash.h>

#include "StdHelpers.h"

#include <C4Game.h>
#ifdef C4ENGINE
#include <C4Def.h>
#include <C4Object.h>
#include <C4Log.h>
#endif
#include <C4Wrappers.h>

#include <string>
#include <vector>
#include <unordered_map>


C4HudBar::C4HudBar() noexcept
  : value{0}, max{1000000}, visible{true}
{}

C4HudBar::C4HudBar(int32_t _value, int32_t _max, bool _visible) noexcept
	: value{_value}, max{_max}, visible{_visible}
{}

bool C4HudBar::operator==(const C4HudBar &rhs) const noexcept
{
	return value == rhs.value && max == rhs.max && visible == rhs.visible;
}

void C4HudBar::CompileFunc(StdCompiler *comp)
{
	comp->Value(mkNamingAdapt(value, "Value", 0));
	comp->Value(mkNamingAdapt(max, "Max", 1000000));
	comp->Value(mkNamingAdapt(visible, "Visible", true));
}


C4HudBars::C4HudBars(std::shared_ptr<const C4HudBarsDef> _def) noexcept : def{_def} {
	for (const auto &bardef : def->bars)
		if (bardef.physical == C4HudBarDef::EBP_None)
			values.emplace_back(bardef.value, bardef.max, bardef.visible);
}

C4HudBar *C4HudBars::BarVal(C4AulContext *cthr, const char *functionName, const std::string &name) {
	try
	{
		const auto index = def->names.at(name);
		auto &bardef = def->bars.at(index);
		if (bardef.value_index >= 0)
		{
			return &values.at(bardef.value_index);
		}
		else
		{
			throw C4AulExecError(cthr->Obj, FormatString("%s: bar \"%s\" is based on physical and can not be set directly.", functionName, name.c_str()).getData());
		}
	}
	catch (const std::out_of_range &)
	{
		throw C4AulExecError(cthr->Obj, FormatString("%s: bar \"%s\" was not defined.", functionName, name.c_str()).getData());
	}

	// should never get here
	throw C4AulExecError(cthr->Obj, FormatString("%s: bar \"%s\" an unexpected error occured.", functionName, name.c_str()).getData());
	return nullptr;
}

void C4HudBars::SetHudBarValue(C4AulContext *cthr, const std::string &name, int32_t value, int32_t max)
{
	auto *barval = BarVal(cthr, "SetHudBar", name);
	barval->value = value;
	if(max > 0) barval->max = max;
}

void C4HudBars::SetHudBarVisibility(C4AulContext *cthr, const std::string &name, bool visible)
{
	auto *barval = BarVal(cthr, "SetHudBarVisible", name);
	barval->visible = visible;
}

void C4HudBars::DrawHudBars(C4Facet &cgo, C4Object &obj) const noexcept
{
	bool needsAdvance = false;
	int32_t maxWidth = 0;

	for (const auto &bardef : def->bars)
	{
		int32_t value = 0;
		int32_t max   = 0;
		bool visible = true;
		const bool hideHUDBars = bardef.hide & C4HudBarDef::EBH_HideHUDBars;

		switch(bardef.physical)
		{
		case C4HudBarDef::EBP_Energy:
			if (hideHUDBars && obj.Def->HideHUDBars & C4Def::HB_Energy) visible = false;
			value = obj.Energy;
			max = obj.GetPhysical()->Energy;
			break;
		case C4HudBarDef::EBP_Breath:
			if (hideHUDBars && obj.Def->HideHUDBars & C4Def::HB_Breath) visible = false;
			value = obj.Breath;
			max = obj.GetPhysical()->Breath;
			break;
		case C4HudBarDef::EBP_Magic:
			if (hideHUDBars && obj.Def->HideHUDBars & C4Def::HB_MagicEnergy) visible = false;
			// draw in units of MagicPhysicalFactor, so you can get a full magic energy bar by script even if partial magic energy training is not fulfilled
			value = obj.MagicEnergy / MagicPhysicalFactor;
			max = obj.GetPhysical()->Magic / MagicPhysicalFactor;
			break;
		default:
			auto &barval = values.at(bardef.value_index);
			value = barval.value;
			max   = barval.max;
			visible = barval.visible;
			break;
		}

		if (bardef.hide & C4HudBarDef::EBH_Empty && value == 0)   visible = false;
		if (bardef.hide & C4HudBarDef::EBH_Full  && value >= max) visible = false;

		if (!visible)
		{
			if (needsAdvance && bardef.advance)
			{
				cgo.X += maxWidth;
				maxWidth = 0;
				needsAdvance = false;
			}
			continue;
		}

		const int32_t width = bardef.facet->Wdt;
		cgo.Wdt = width;
		cgo.DrawEnergyLevelEx(value, max, *bardef.facet, bardef.index, bardef.scale);

		maxWidth = std::max<int32_t>(maxWidth, width+1);
		if (bardef.advance)
		{
			cgo.X += maxWidth;
			maxWidth = 0;
			needsAdvance = false;
		}
		else
		{
			needsAdvance = true;
		}
	}
}


C4HudBarDef::C4HudBarDef() noexcept :
	name{}, physical{EBP_None}, hide{EBH_Empty},
	gfx{}, facet{}, index{}, advance{true},
	value_index{-1}, value{0}, max{1000000}, visible{true}, scale{1.0f}
{}

C4HudBarDef::C4HudBarDef(std::string_view _name, std::string_view _gfx, const std::shared_ptr<C4FacetExID> &_facet, int32_t _index, Physical _physical) noexcept :
	name{_name}, physical{_physical}, hide{DefaultHide(physical)},
	gfx{_gfx}, facet{_facet}, index{_index}, advance{true},
	value_index{-1}, value{0}, max{1000000}, visible{true}, scale{1.0f}
{}

bool C4HudBarDef::operator==(const C4HudBarDef &rhs) const noexcept
{
	return
		name == rhs.name &&
		physical == rhs.physical &&
		hide == rhs.hide &&
		gfx == rhs.gfx &&
		facet == rhs.facet &&
		index == rhs.index &&
		advance == rhs.advance &&
		value == rhs.value &&
		max == rhs.max &&
		visible == rhs.visible;
}

C4HudBarDef::Hide C4HudBarDef::DefaultHide(C4HudBarDef::Physical physical) noexcept {
	switch (physical)
	{
	case EBP_Energy: return EBH_Never | EBH_HideHUDBars;
	case EBP_Breath: return EBH_Full | EBH_HideHUDBars;
	case EBP_Magic:
	default:
		return EBH_Empty | EBH_HideHUDBars;
	}
}

int32_t C4HudBarDef::DefaultIndex(C4HudBarDef::Physical physical) noexcept {
	switch (physical)
	{
	case EBP_Energy: return 0;
	case EBP_Magic:  return 1;
	case EBP_Breath: return 2;
	default:
		return 0;
	}
}

std::size_t C4HudBarDef::GetHash() const noexcept
{
	std::size_t result = std::hash<std::string>{}(name);
	HashCombine(result, std::hash<int32_t>{}(physical));
	HashCombine(result, std::hash<int32_t>{}(hide));
	HashCombine(result, std::hash<std::string>{}(gfx));
	HashCombine(result, std::hash<int32_t>{}(index));
	HashCombine(result, std::hash<bool>{}(advance));
	HashCombine(result, std::hash<int32_t>{}(value_index));
	HashCombine(result, std::hash<int32_t>{}(value));
	HashCombine(result, std::hash<int32_t>{}(max));
	HashCombine(result, std::hash<bool>{}(visible));
	return result;
}

void C4HudBarDef::CompileFunc(StdCompiler *comp)
{
	comp->Value(mkNamingAdapt(name, "Name"));
	StdEnumEntry<Physical> PhysicalEntries[] =
	{
		{ "",       EBP_None },
		{ "Energy", EBP_Energy },
		{ "Magic",  EBP_Magic },
		{ "Breath", EBP_Breath },
	};
	comp->Value(mkNamingAdapt(mkEnumAdaptT<uint8_t>(physical, PhysicalEntries), "Physical", EBP_None));
	StdEnumEntry<Hide> HideEntries[] =
	{
		{ "Never",             EBH_Never },
		{ "Never_HideHud",     EBH_Never | EBH_HideHUDBars },
		{ "Empty",             EBH_Empty },
		{ "Empty_HideHud",     EBH_Empty | EBH_HideHUDBars },
		{ "Full",              EBH_Full },
		{ "Full_HideHud",      EBH_Full | EBH_HideHUDBars },
		{ "EmptyFull",         EBH_EmptyFull },
		{ "EmptyFull_HideHud", EBH_EmptyFull | EBH_HideHUDBars },
	};
	comp->Value(mkNamingAdapt(mkEnumAdaptT<uint8_t>(hide, HideEntries), "Hide", EBH_Empty));
	comp->Value(mkNamingAdapt(gfx, "Gfx"));
	comp->Value(mkNamingAdapt(index, "Index"));
	comp->Value(mkNamingAdapt(advance, "Advance", true));
	comp->Value(mkNamingAdapt(value_index, "ValueIndex", -1));
	comp->Value(mkNamingAdapt(value, "Value", 0));
	comp->Value(mkNamingAdapt(max, "Max", 1000000));
	comp->Value(mkNamingAdapt(visible, "Visible", true));
	// gfx and scale are restored from def.gfxs
}

inline const C4HudBarDef::Hide operator|(const C4HudBarDef::Hide a, const C4HudBarDef::Hide b)
{
	return static_cast<const C4HudBarDef::Hide>(static_cast<const int32_t>(a) | static_cast<const int32_t>(b));
}


namespace
{
	template <typename Map>
	bool MapCompare (Map const &lhs, Map const &rhs) noexcept
	{
		return lhs.size() == rhs.size()
			&& std::equal(lhs.begin(), lhs.end(),
										rhs.begin());
	}
}

C4HudBarsDef::Gfx::Gfx() noexcept : key{}, file{}, amount{0}, scale{0} {}

C4HudBarsDef::Gfx::Gfx(std::string k, std::string f, int32_t _a, int32_t _s) noexcept : key{k}, file{f}, amount{_a}, scale{_s} {}

bool C4HudBarsDef::Gfx::operator==(const Gfx &rhs) const noexcept
{
	return key == rhs.key && file == rhs.file && amount == rhs.amount && scale == rhs.scale;
}

void C4HudBarsDef::Gfx::CompileFunc(StdCompiler *comp)
{
	comp->Value(mkNamingAdapt(key, "Key"));
	comp->Value(mkNamingAdapt(file, "File"));
	comp->Value(mkNamingAdapt(amount, "Amount"));
	comp->Value(mkNamingAdapt(scale, "Scale"));
}

C4HudBarsDef::C4HudBarsDef(const Gfxs &_gfxs, const Bars &_bars) : gfxs{_gfxs}, bars{_bars}
{
	PopulateNamesFromValues([=](StdStrBuf msg){LogFatal(msg.getData());}, bars, names);
}

C4HudBarsDef::C4HudBarsDef(const Gfxs &_gfxs, const C4HudBarsDef::Bars &_bars, const C4HudBarsDef::Names &_names) noexcept : gfxs{_gfxs}, bars{_bars}, names{_names}
{
}

void C4HudBarsDef::PopulateNamesFromValues(const std::function<void(StdStrBuf)> &error, const C4HudBarsDef::Bars &bars, C4HudBarsDef::Names &names)
{
	int32_t i = 0;
	for (const auto &bar : bars)
	{
		const auto success = names.emplace(bar.name, i);
		if (!success.second)
		{
			error(FormatString("C4HudBarsDef %s definition, names must be unique, duplicate detected", bar.name.c_str()));
		}
		++i;
	}
}

bool C4HudBarsDef::operator==(const C4HudBarsDef &rhs) const noexcept
{
	return MapCompare(gfxs, rhs.gfxs) && bars == rhs.bars;
}

std::size_t C4HudBarsDef::GetHash() const noexcept
{
	std::size_t result = 0;
	for (const auto &gfx : gfxs)
	{
		HashCombine(result, std::hash<std::string>{}(gfx.second.key));
		HashCombine(result, std::hash<std::string>{}(gfx.second.file));
		HashCombine(result, std::hash<uint32_t>{}(gfx.second.amount));
		HashCombine(result, std::hash<uint32_t>{}(gfx.second.scale));
	}

	for (const auto &bardef : bars)
		HashCombine(result, bardef.GetHash());

	return result;
}

std::size_t std::hash<const C4HudBarsDef>::operator()(const C4HudBarsDef &value) const noexcept
{
	return value.GetHash();
}


std::shared_ptr<C4HudBars> C4HudBarsUniquifier::DefaultBars()
{
	if(!defaultBars)
	{
		const auto file = "EnergyBars";
		const auto gfxs = C4HudBarsDef::Gfxs{{file, C4HudBarsDef::Gfx{file, file, 3, 100}}};
		const auto gfx = GetFacet([=](StdStrBuf msg){LogFatal(FormatString("could not load DefaultBars \"%s\"", file).getData());}, gfxs, file);
		const auto def = UniqueifyDefinition(
			std::move(std::make_unique<C4HudBarsDef>(
		    gfxs,
		    C4HudBarsDef::Bars{
		      C4HudBarDef{"Energy", file, gfx, 0, C4HudBarDef::EBP_Energy},
		      C4HudBarDef{"Magic",  file, gfx, 1, C4HudBarDef::EBP_Magic},
		      C4HudBarDef{"Breath", file, gfx, 2, C4HudBarDef::EBP_Breath}
		})));
		defaultBars = Instantiate(def);
	}

	return defaultBars;
}

std::shared_ptr<C4FacetExID> C4HudBarsUniquifier::GetFacet(const std::function<void(StdStrBuf)> &error, const C4HudBarsDef::Gfxs &gfxs, std::string_view gfx)
{
	const std::string key(gfx);

	try
	{
		const auto facet = graphics.at(key).lock();
		if(facet) return facet;
	}
	catch (const std::out_of_range &)
	{
		// handled by code below
	}

	// facet needs to be loaded
	int32_t amount = 0;
	int32_t scale  = 100;
	std::string file;

	try
	{
		auto gfx = gfxs.at(key);
		amount = gfx.amount;
		scale = gfx.scale;
		file = gfx.file;
	}
	catch (const std::out_of_range &)
	{
		error(FormatString("missing key \"%s\" in graphics definition", key.c_str()));
		return nullptr;
	}

	// TODO FIXME why wasn't this already called before we get here?
	Game.GraphicsResource.RegisterMainGroups();

	const auto deleter = [=](C4FacetExID *facet)
	{
		graphics.erase(key);
		delete facet;
	};
	const auto facet = std::shared_ptr<C4FacetExID>(new C4FacetExID(), deleter);
	const bool success = Game.GraphicsResource.LoadFile(*facet, file.c_str(), Game.GraphicsResource.Files);
	if(!success)
	{
		error(FormatString("could not load custom energy bar graphic \"%s\"", file.c_str()));
		return nullptr;
	}

	const int32_t barWdt = facet->Surface->Wdt / (amount * 2);
	const int32_t barHgt = facet->Surface->Hgt / 3;
	const int32_t scaledWdt = (barWdt*100) / scale;
	const int32_t scaledHgt = (barHgt*100) / scale;
	facet->Set(facet->Surface, 0, 0, scaledWdt, scaledHgt);

	graphics.emplace(key, std::weak_ptr<C4FacetExID>(facet));
	return facet;
}

std::shared_ptr<const C4HudBarsDef> C4HudBarsUniquifier::UniqueifyDefinition(std::unique_ptr<C4HudBarsDef> definition)
{
	// definition always gets owned by a shared_ptr,
	// that either ends up being the canonical shared_ptr,
	// or goes out of scope deleting the definition.
	// the weak ptr remembers the custom deleter
	const auto deleter = [=](const C4HudBarsDef *def)
	{
		definitions.erase(*def);
		delete def;
	};
	const auto shared = std::shared_ptr<const C4HudBarsDef>(definition.release(), deleter);
	const auto it_success = definitions.emplace(*shared.get(), std::weak_ptr<const C4HudBarsDef>(shared));
	if (!it_success.second)
	{
		// definition already existed, create shared ptr from existing weak_ptr
		return it_success.first->second.lock();
	}
	return shared;
}

std::shared_ptr<C4HudBars> C4HudBarsUniquifier::Instantiate(std::shared_ptr<const C4HudBarsDef> definition)
{
	if(definition == nullptr) return nullptr;
	return std::make_shared<C4HudBars>(definition);
}

std::shared_ptr<C4HudBars> C4HudBarsUniquifier::DefineHudBars(C4AulContext *cthr, C4ValueHash &graphics, const C4ValueArray &definition)
{
	int32_t value_index = 0;
	C4HudBarsDef::Gfxs gfx;
	C4HudBarsDef::Bars bars;
	C4HudBarsDef::Names names;

	ProcessGraphics(cthr, graphics, gfx);
	ProcessGroup(cthr, value_index, gfx, definition, bars, true);
	const auto error = [=](StdStrBuf msg){throw C4AulExecError(cthr->Obj, FormatString("DefineHudBars: %s", msg.getData()).getData());};
	C4HudBarsDef::PopulateNamesFromValues(error, bars, names);

	auto def = UniqueifyDefinition(std::move(std::make_unique<C4HudBarsDef>(gfx, bars, names)));
	return Instantiate(def);
}

void C4HudBarsUniquifier::ProcessGraphics(C4AulContext *cthr, C4ValueHash &map, C4HudBarsDef::Gfxs &gfx)
{
	const auto keyAmount = C4VString("amount");
	const auto keyScale  = C4VString("scale");
	const auto keyFile   = C4VString("file");

	for (auto &[key, val] : map)
	{
		if (key.GetType() != C4V_String) {
			throw C4AulExecError(cthr->Obj, "DefineHudBars: keys within maps are expected to be of type string");
		}
		const auto keyStr = key.GetRefVal()._getStr();
		const auto _key = keyStr->Data.getData();

		if (val.GetType() != C4V_Map)
		{
			throw C4AulExecError(cthr->Obj, FormatString("DefineHudBars: key \"%s\" is not a map, got: %s", _key, val.GetDataString().getData()).getData());
		}
		const auto _val = val.GetRefVal()._getMap();
		const auto &m = *_val;

		C4Value file = m[keyFile];
		auto _file = _key;
		if (file != C4VNull) _file = file.getStr()->Data.getData();

		auto _amount = m[keyAmount];
		auto _scale  = m[keyScale];
		int32_t amount = _amount.getInt();
		int32_t scale  = _scale.getInt();
		if (amount == 0) amount = 1;
		if (scale == 0) scale = 100;

		const auto it_success = gfx.emplace(_key, C4HudBarsDef::Gfx{_key, _file, amount, scale});
		if (!it_success.second)
		{
			throw C4AulExecError(cthr->Obj, FormatString("DefineHudBars: duplicate key \"%s\" in gfx description ", _key).getData());
		}
	}
}

void C4HudBarsUniquifier::ProcessGroup(C4AulContext *cthr, int32_t &value_index, const C4HudBarsDef::Gfxs &graphics, const C4ValueArray &group, C4HudBarsDef::Bars &bars, bool advanceAlways)
{
	const auto error = [&](const char *msg, C4Value &val) {
		auto format = std::string("DefineHudBars: ") + msg;
		throw C4AulExecError(cthr->Obj, FormatString(format.c_str(), val.GetDataString().getData()).getData());
	};

	const int32_t size = group.GetSize();
	for (int32_t i = 0; i < size; ++i)
	{
		C4Value element = group[i];
		switch (element.GetType())
		{
		case C4V_Map:
			if (const auto *map = element.getMap(); map)
			{
				ProcessHudBar(cthr, value_index, graphics, *map, bars, advanceAlways || i == size-1);
			}
			else
			{
				error("got unexpected value: %s", element);
			}
			break;
		case C4V_Array:
			if(advanceAlways)
			{
				if (const auto *array = element.getArray(); array)
				{
					ProcessGroup(cthr, value_index, graphics, *array, bars, false);
				}
				else
				{
					error("got unexpected value: %s", element);
				}
			}
			else
			{
				error("groups in groups are not allowed: %s", element);
			}
			break;

		default:
			error("array or map expected, got: %s", element);
		}
	}
}

void C4HudBarsUniquifier::ProcessHudBar(C4AulContext *cthr, int32_t &value_index, const C4HudBarsDef::Gfxs &graphics, const C4ValueHash &bar, C4HudBarsDef::Bars &bars, bool advance)
{
	C4Value name = bar[C4VString("name")];
	auto *_name = name.getStr();
	if (!_name)
	{
		throw C4AulExecError(cthr->Obj, FormatString("DefineHudBars: HudBar definition has invalid name, got: %s", name.GetDataString().getData()).getData());
	}

	const auto error = [&](const char *property, C4Value &val) {
		auto format = std::string("DefineHudBars: \"%s\" definition has invalid ") + property + ", got %s";
		throw C4AulExecError(cthr->Obj, FormatString(format.c_str(), _name->Data.getData(), val.GetDataString().getData()).getData());
	};

	C4Value gfx = bar[C4VString("gfx")];
	auto *_gfx = gfx.getStr();
	if (!_gfx) error("gfx", gfx);

	C4Value physical = bar[C4VString("physical")];
	auto _physical = static_cast<C4HudBarDef::Physical>(physical.getInt());
	if (_physical & ~C4HudBarDef::EBP_All) error("physical", physical);

	C4Value hide = bar[C4VString("hide")];
	auto _hide = C4HudBarDef::EBH_Empty;
	if (hide != C4VNull) _hide = static_cast<C4HudBarDef::Hide>(hide.getInt());
	if (_hide & ~C4HudBarDef::EBH_All) error("hide", hide);

	C4Value index = bar[C4VString("index")];
	C4Value value = bar[C4VString("value")];
	C4ValueInt _index = index.getInt();
	C4ValueInt _value = value.getInt();
	if (_index < 0) error("index", index);
	if (_value < 0) error("value", value);

	C4ValueInt _max = 1000000;
	if (bar.contains(C4VString("max")))
	{
		auto max = bar[C4VString("max")];
		_max = max.getInt();
		if (_max < 0) error("max", max);
	}

	bool _visible = true;
	if (bar.contains(C4VString("visible")))
	{
		auto visible = bar[C4VString("visible")];
		_visible = visible.getBool();
	}

	{
		const auto file = _gfx->Data.getData();
		const auto facetError = [&](StdStrBuf msg)
		{
			throw C4AulExecError(cthr->Obj, FormatString("DefineHudBars %s %s", _name->Data.getData(), msg.getData()).getData());
		};
		const auto facet = GetFacet(facetError, graphics, file);

		C4HudBarDef bar(_name->Data.getData(), file, facet, _index, _physical);
		if(physical != C4VNull)
		{
			bar.physical = _physical;
			bar.hide = C4HudBarDef::DefaultHide(_physical);
		}
		else
		{
			bar.value_index = value_index++;
		}
		if(hide != C4VNull) bar.hide = _hide;
		bar.value = _value;
		bar.max = _max;
		bar.visible = _visible;
		bar.advance = advance;
		auto scale = graphics.at(file).scale;
		bar.scale = static_cast<float>(scale) / 100.0f;
		bars.push_back(bar);
	}
}


void C4HudBarsAdapt::CompileFunc(StdCompiler *comp) {
	bool compiler = comp->isCompiler();

	if (!compiler)
	{
		if (bars)
		{
			std::vector<C4HudBarsDef::Gfx> temp;
			for (const auto it : bars->def->gfxs)
				temp.emplace_back(it.second);

			comp->Value(mkNamingAdapt(mkSTLContainerAdapt(temp), "Gfx", std::vector<C4HudBarsDef::Gfx>{}));

      auto *def = const_cast<C4HudBarsDef*>(bars->def.get());
			comp->Value(mkNamingAdapt(mkSTLContainerAdapt(def->bars), "Def", C4HudBarsDef::Bars{}));
			comp->Value(mkNamingAdapt(mkSTLContainerAdapt(bars->values), "Bar", std::vector<C4HudBar>{}));
		}

	}
	else
	{
		auto def = std::make_unique<C4HudBarsDef>();
		{
			C4HudBarsDef::Gfxs gfxs;
			std::vector<C4HudBarsDef::Gfx> temp;
			comp->Value(mkNamingAdapt(mkSTLContainerAdapt(temp), "Gfx", std::vector<C4HudBarsDef::Gfx>{}));
			for (const auto &gfx : temp)
				gfxs.emplace(gfx.key, gfx);

			C4HudBarsDef::Bars bars;
			comp->Value(mkNamingAdapt(mkSTLContainerAdapt(bars), "Def", C4HudBarsDef::Bars{}));

			def->gfxs.swap(gfxs);
			def->bars.swap(bars);
		}

		// get facets and restore scale from gfxs
		const auto facetError = [=](StdStrBuf msg){comp->Warn("Error loading HudBars %s", msg.getData());};
		for (auto &bar : def->bars)
		{
			bar.facet = Game.HudBars.GetFacet(facetError, def->gfxs, bar.gfx.c_str());
			if (bar.facet == nullptr) {
				bars = Game.HudBars.DefaultBars();
				return;
			}
			const auto scale = def->gfxs.at(bar.gfx).scale;
			bar.scale = static_cast<float>(scale) / 100.0f;
		}

		const auto uniq_def = Game.HudBars.UniqueifyDefinition(std::move(def));
		const auto instance = Game.HudBars.Instantiate(uniq_def);
		comp->Value(mkNamingAdapt(mkSTLContainerAdapt(instance->values), "Bar", std::vector<C4HudBar>{}));

		bars = instance;
	}
}

